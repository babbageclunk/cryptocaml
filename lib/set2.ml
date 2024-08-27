let pkcs7 size text =
  let last_block = (Bytes.length text) mod size in
  if last_block = 0 then text
  else
    let padding = size - last_block in
    Char.chr padding
    |> Bytes.make padding
    |> Bytes.cat text

let key = Bytes.of_string "YELLOW SUBMARINE"

let set2c1 () =
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

let set2c2 () =
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
  if Random.bool () then
    Common.aes_ecb rand_key padded
  else
    aes_cbc_encrypt rand_key rand_iv padded
