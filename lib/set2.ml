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

let aes_cbc_decrypt key iv text =
  let size = Bytes.length key in
  let padded = pkcs7 size text in
  let blocks = Common.blocks size padded in
  let rec chain last blocks acc =
    match blocks with
    | [] -> List.rev acc |> Bytes.concat Bytes.empty
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
    | [] -> List.rev acc |> Bytes.concat Bytes.empty
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
    Bytes.concat Bytes.empty [randpadding (); text; randpadding ()]
    |> pkcs7 16
  in
  if Random.bool () then (
    Printf.printf "using ECB\n";
    Common.aes_ecb rand_key padded
  ) else (
    Printf.printf "using CBC\n";
    aes_cbc_encrypt rand_key rand_iv padded
  )

let set2c11 () =
  Random.self_init ();
  let input =
    List.init 3 (fun _ -> key)
    |> Bytes.concat Bytes.empty
  in
  let encrypted = encryption_oracle input in
  match (Common.find_dupe_blocks 16 encrypted) with
  | [] -> "CBC detected"
  | _ -> "ECB detected"


let unknown_text = (
  "Um9sbGluJyBpbiBteSA1LjAKV2l0aCBteSByYWctdG9wIGRvd24gc28gbXkg\
   aGFpciBjYW4gYmxvdwpUaGUgZ2lybGllcyBvbiBzdGFuZGJ5IHdhdmluZyBq\
   dXN0IHRvIHNheSBoaQpEaWQgeW91IHN0b3A/IE5vLCBJIGp1c3QgZHJvdmUg\
   YnkK"
)

let consistent_key : string option ref = ref None

let get_consistent_key () =
  match !consistent_key with
  | None ->
    let key = Bytes.init 16 randchar in
    consistent_key := Some key;
    key
  | Some k -> k

let ecb_oracle text =
  let key = get_consistent_key () in
  something
