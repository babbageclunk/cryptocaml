let pkcs7 size text =
  let last_block = (Bytes.length text) mod size in
  if last_block = 0 then text
  else
    let padding = size - last_block in
    Char.chr padding
    |> Bytes.make padding
    |> Bytes.cat text

let key = Bytes.of_string "YELLOW SUBMARINE"

let set2c9 () =
  pkcs7 20 key |> Bytes.to_string

let join_bytes blist =
  Bytes.concat Bytes.empty blist

let aes_cbc_decrypt key iv text =
  let size = Bytes.length key in
  let padded = pkcs7 size text in
  let blocks = Common.blocks size padded in
  let rec chain last blocks acc =
    match blocks with
    | [] -> List.rev acc |> join_bytes
    | b::bs ->
      let this = Common.aes_ecb key b |> Common.xor_bytes last in
      chain b bs (this::acc)
  in
  chain iv blocks []

let set2c10 () =
  Common.input ()
  |> aes_cbc_decrypt key (Bytes.make 16 '\000')
  |> Bytes.to_string


let aes_cbc_encrypt key iv text =
  let size = Bytes.length key in
  let padded = pkcs7 size text in
  let blocks = Common.blocks size padded in
  let rec chain last blocks acc =
    match blocks with
    | [] -> List.rev acc |> join_bytes
    | b::bs ->
      let this = Common.xor_bytes last b |> Common.aes_ecb key in
      chain this bs (this::acc)
  in
  chain iv blocks []

let randchar _ =
  Random.int 256 |> Char.chr

let randpadding () =
  let size = Random.int_in_range ~min:5 ~max:10 in
  Bytes.init size randchar

let encryption_oracle text =
  let rand_key = Bytes.init 16 randchar in
  let rand_iv = Bytes.init 16 randchar in
  let padded =
    join_bytes [randpadding (); text; randpadding ()]
    |> pkcs7 16
  in
  if Random.bool () then (
    Printf.printf "using ECB\n";
    Common.aes_ecb rand_key padded
  ) else (
    Printf.printf "using CBC\n";
    aes_cbc_encrypt rand_key rand_iv padded
  )

let has_dupe_blocks size text =
  Common.find_dupe_blocks size text |> List.is_empty |> not


let set2c11 () =
  Random.self_init ();
  let input =
    List.init 3 (fun _ -> key)
    |> join_bytes
  in
  let encrypted = encryption_oracle input in
  if (has_dupe_blocks 16 encrypted) then
    "ECB detected"
  else
    "CBC detected"

let unknown_text = (
  "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
   aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
   dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
   YnkK"
)

let consistent_key : bytes option ref = ref None

let get_consistent_key () =
  match !consistent_key with
  | None ->
    let key = Bytes.init 16 randchar in
    consistent_key := Some key;
    key
  | Some k -> k

let ecb_oracle text =
  let key = get_consistent_key () in
  let suffix = Common.b64decode unknown_text in
  Bytes.cat text suffix
  |> pkcs7 16
  |> Common.aes_ecb key

let find_block_size oracle =
  let prefix size text = Bytes.sub text 0 size in
  let rec check size last =
    if size > 1000 then
      raise Not_found;
    let result = Bytes.make (size + 1) 'A' |> oracle in
    let prev_prefix = prefix size last in
    let this_prefix = prefix size result in
    if prev_prefix = this_prefix then
      size
    else
      check (size + 1) result
  in
  Bytes.of_string "A" |> oracle |> check 1

let find_next_byte oracle block_size known =
  let known_size = Bytes.length known in
  let padding_size = block_size - (known_size mod block_size) - 1 in
  let padding = Bytes.make padding_size 'A' in
  let encrypted = oracle padding in
  let blocks = Common.blocks block_size encrypted in
  let check_block = known_size / block_size in
  let target_block = List.nth blocks check_block in

  Printf.printf "padding_size = %d, check_block = %d, target_block = %S\n" padding_size check_block (Common.hexencode target_block);

  let rec check_from candidate =
    let cand_char = Char.chr candidate in
    let my_padding = join_bytes [padding; known; Bytes.make 1 cand_char] in
    let encrypted = oracle my_padding in
    let blocks = Common.blocks block_size encrypted in
    let my_block = List.nth blocks check_block in
    Printf.printf "cand_char = %C, my_padding = %S, my_block = %S\n" cand_char (Bytes.to_string my_padding) (Common.hexencode my_block);
    if my_block = target_block then
      cand_char
    else
      check_from (candidate + 1)
  in
  check_from 0

exception Not_ecb

let set2c12 () =
  let block_size = find_block_size ecb_oracle in
  let encrypted = Bytes.make (block_size * 2) 'A' |> ecb_oracle in
  if not (has_dupe_blocks block_size encrypted) then
    raise Not_ecb;
  "write me"
