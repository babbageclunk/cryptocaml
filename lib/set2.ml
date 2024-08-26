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

let aes_cbc key iv text =
  let size = Bytes.length key in
  let padded = pkcs7 size text in
  let blocks = Common.blocks size padded in
  List.to_seq blocks
