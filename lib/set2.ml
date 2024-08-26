let pkcs7 size block =
  let last_block = (Bytes.length block) mod size in
  if last_block = 0 then block
  else
    let padding = size - last_block in
    Char.chr padding
    |> Bytes.make padding
    |> Bytes.cat block

let key = Bytes.of_string "YELLOW SUBMARINE"

let set2c1 () =
  pkcs7 20 key |> Bytes.to_string
