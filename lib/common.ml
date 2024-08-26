let aes_ecb key text =
  let cipher = Bytes.to_string key
      |> new Cryptokit.Block.aes_decrypt
      |> new Cryptokit.Block.cipher
  in
  Bytes.to_string text |> cipher#put_string;
  cipher#finish;
  cipher#get_string |> Bytes.of_string

let blocks size text =
  let text_len = Bytes.length text in
  let rec bsplit acc pos =
    let remaining = text_len - pos in
    if remaining > size then
      let chunk = Bytes.sub text pos size in
      bsplit (chunk :: acc) (pos + size)
    else
      (Bytes.sub text pos remaining)::acc
  in
  bsplit [] 0 |> List.rev

let xor_char a b = (Char.code a) lxor (Char.code b) |> Char.chr

let xor_bytes a b =
  Seq.map2 xor_char (Bytes.to_seq a) (Bytes.to_seq b) |> Bytes.of_seq

(* Base64 doesn't handle \n in the input so we need to filter those
   out first. *)
let b64decode str =
  let r = Str.regexp "\n" in
  Str.global_replace r "" str
  |> Base64.decode_exn
  |> Bytes.of_string

let input () =
  In_channel.input_all In_channel.stdin |> b64decode
