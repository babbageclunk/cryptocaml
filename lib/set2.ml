let pkcs7_pad size text =
  let last_block = (Bytes.length text) mod size in
  if last_block = 0 then text
  else
    let padding = size - last_block in
    Char.chr padding
    |> Bytes.make padding
    |> Bytes.cat text

let all_bytes_match char text =
  Bytes.to_seq text
  |> Seq.for_all (fun c -> (c = char))

let pkcs7_unpad size text =
  let length = Bytes.length text in
  if (length mod size) != 0 then
    text
  else
    let padding_char = Bytes.get text (length - 1) in
    let padding_len = Char.code padding_char in
    if padding_len > (size - 1) then
      text
    else
      let maybe_padding = Bytes.sub text (length - padding_len) padding_len in
      if all_bytes_match padding_char maybe_padding then
        Bytes.sub text 0 (length - padding_len)
      else
        text

let key = Bytes.of_string "YELLOW SUBMARINE"

let set2c9 () =
  pkcs7_pad 20 key |> Bytes.to_string

let join_bytes blist =
  Bytes.concat Bytes.empty blist

let aes_cbc_decrypt key iv text =
  let size = Bytes.length key in
  let padded = pkcs7_pad size text in
  let blocks = Common.blocks size padded in
  let rec chain last blocks acc =
    match blocks with
    | [] -> List.rev acc |> join_bytes
    | b::bs ->
      let this = Common.aes_ecb_decrypt key b |> Common.xor_bytes last in
      chain b bs (this::acc)
  in
  chain iv blocks []

let set2c10 () =
  Common.input ()
  |> aes_cbc_decrypt key (Bytes.make 16 '\000')
  |> Bytes.to_string


let aes_cbc_encrypt key iv text =
  let size = Bytes.length key in
  let padded = pkcs7_pad size text in
  let blocks = Common.blocks size padded in
  let rec chain last blocks acc =
    match blocks with
    | [] -> List.rev acc |> join_bytes
    | b::bs ->
      let this = Common.xor_bytes last b |> Common.aes_ecb_encrypt key in
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
    |> pkcs7_pad 16
  in
  if Random.bool () then (
    Printf.printf "using ECB\n";
    Common.aes_ecb_encrypt rand_key padded
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
  |> pkcs7_pad 16
  |> Common.aes_ecb_encrypt key

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

  (* Printf.printf "padding_size = %d, check_block = %d, target_block = %S\n" padding_size check_block (Common.hexencode target_block); *)

  let rec check_from candidate =
    if candidate > 255 then
      (* This happens when we hit padding - stop! *)
      None
    else
      let cand_char = Char.chr candidate in
      let my_padding = join_bytes [padding; known; Bytes.make 1 cand_char] in
      let encrypted = oracle my_padding in
      let blocks = Common.blocks block_size encrypted in
      let my_block = List.nth blocks check_block in
      (* Printf.printf "cand_char = %C, my_padding = %S, my_block = %S\n" cand_char (Bytes.to_string my_padding) (Common.hexencode my_block); *)
      if my_block = target_block then
        Some cand_char
      else
        check_from (candidate + 1)
  in
  check_from 0

let find_unknown_suffix oracle block_size =
  let rec loop known =
    match find_next_byte oracle block_size known with
    | None -> known
    | Some c ->
      Bytes.make 1 c
      |> Bytes.cat known
      |> loop
  in
  loop Bytes.empty

exception Not_ecb

let set2c12 () =
  let block_size = find_block_size ecb_oracle in
  let encrypted = Bytes.make (block_size * 2) 'A' |> ecb_oracle in
  if not (has_dupe_blocks block_size encrypted) then
    raise Not_ecb;

  find_unknown_suffix ecb_oracle block_size |> Bytes.to_string


let parse_profile text =
  String.split_on_char '&' text
  |> List.filter_map (
    fun item ->
      match String.split_on_char '=' item with
      | a::b::_ -> Some (a, b)
      | _ -> None
  )

let remove_meta_chars text =
  String.to_seq text
  |> Seq.filter (fun c -> c != '&' && c != '=')
  |> String.of_seq

let profile_for email =
  let cleaned = remove_meta_chars email in
  Printf.sprintf "email=%s&uid=10&role=user" cleaned

let encrypt_profile profile =
  let key = get_consistent_key () in
  Bytes.of_string profile
  |> pkcs7_pad 16
  |> Common.aes_ecb_encrypt key

let tee f x = f x; x

let decrypt_profile ciphertext =
  let key = get_consistent_key () in
  Common.aes_ecb_decrypt key ciphertext
  |> pkcs7_unpad 16
  |> Bytes.to_string
  |> parse_profile

(* So now we can make an admin profile by making two profiles:
   - one with the email "abc@efghijklm" - that produces the profile
     "email=abc@efghijklm&uid=10&role=user"
          block 3 starts here --------^

   - the other with the email "0123456789admin\011\011\011\011\011\011\011\011\011\011\011"
     produces the profile
     "email=0123456789admin\011\011\011\011\011\011\011\011\011\011\011&uid=10&role=user"
     Then grab this   |-----------------------------------------------| block from the
     encrypted text and concat it onto the first two blocks of the first encrypted profile.
*)

let set2c13 () =
  let e1 =
    profile_for "abc@efghijklm"
    |> encrypt_profile
  in
  let e2 =
    profile_for "0123456789admin\011\011\011\011\011\011\011\011\011\011\011"
    |> encrypt_profile
  in
  Bytes.cat (Bytes.sub e1 0 32) (Bytes.sub e2 16 16)
  |> decrypt_profile
  |> List.assoc "role"

let consistent_prefix : bytes option ref = ref None

let get_consistent_prefix () =
  match !consistent_prefix with
  | None ->
    let prefix_len = Random.int_in_range ~min:1 ~max:64 in
    let prefix = Bytes.init prefix_len randchar in
    consistent_prefix := Some prefix;
    prefix
  | Some p -> p

let ecb_prefix_oracle text =
  let prefix = get_consistent_prefix () in
  let key = get_consistent_key () in
  let suffix = Common.b64decode unknown_text in
  join_bytes [prefix; text; suffix]
  |> pkcs7_pad 16
  |> Common.aes_ecb_encrypt key
