let c1data = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

let hexdecode s = `Hex s |> Hex.to_bytes
let hexencode b = Hex.of_bytes b |> Hex.show

let b64encode bytes = Base64.encode_string (Bytes.to_string bytes)

let set1c1 () = b64encode (hexdecode c1data)

let xor_char a b = (Char.code a) lxor (Char.code b) |> Char.chr

let xor_bytes a b = Seq.map2 xor_char (Bytes.to_seq a) (Bytes.to_seq b) |> Bytes.of_seq

let c2data1 = "1c0111001f010100061a024b53535009181c"
let c2data2 = "686974207468652062756c6c277320657965"
let set1c2 () = xor_bytes (hexdecode c2data1) (hexdecode c2data2) |> hexencode

let c3data = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

let expected_order = Bytes.of_string "etaoin shrdlucmfwypvbgkqjxz"

module CharMap = Map.Make(Char)

let char_frequencies text =
  let counts = CharMap.empty in
  let add_char map c = CharMap.update c (function None -> Some 1 | Some n -> Some (n + 1)) map in
  Bytes.fold_left add_char counts text

let filter_bytes b pred = Bytes.to_seq b |> Seq.filter pred |> Bytes.of_seq

let chars_by_freq text =
  let freqs = Bytes.map Char.lowercase_ascii text |> char_frequencies in
  let sorted_freqs = CharMap.bindings freqs |> List.sort (fun (_, a) (_, b) -> compare b a) in
  List.to_seq sorted_freqs |> Seq.map fst |> Bytes.of_seq

(* Guess at a scoring algo: *)
(* For each char in the expected order, that char is worth a possible
   len(lowercase) - pos points, find the char in the actual order, it
   gets possible points - abs(pos - actual pos points). If the char
   isn't found then 0 *)
let score freq_string =
  let max_char_points = Bytes.length expected_order in
  let score_char expected_pos char =
    match (Bytes.index_from_opt freq_string 0 char) with
    | Some n -> max_char_points - expected_pos - Int.abs (n - expected_pos)
    | None -> 0
  in
  Bytes.to_seq expected_order |> Seq.mapi score_char |> Seq.fold_left (+) 0

type decrypt_score = {
  score : int;
  key : char;
  output : Bytes.t;
}

let decrypt text =
  let xor_with key =
    let key_bytes = Bytes.make (Bytes.length text) key in
    xor_bytes text key_bytes in

  let score_key key =
    let decrypted = xor_with key in
    let result = chars_by_freq decrypted |> score in
    {score=result; key=key; output=decrypted} in

  let all_keys = List.init 256 Char.chr in
  let results = List.map score_key all_keys in
  List.sort compare results |> List.rev |> List.hd

let print_triple {score; key; output} =
  Printf.sprintf "Key: %C, Score: %d, Decoded: %S" key score (Bytes.to_string output)

let set1c3 () =
  hexdecode c3data |> decrypt |> print_triple

let find_encrypted_line ic =
  let compare_line current line =
    let this_result = hexdecode line |> decrypt in
    if (current < this_result) then this_result else current
  in
  In_channel.fold_lines compare_line {score=0; key='\000'; output=Bytes.empty} ic

let set1c4 () =
  find_encrypted_line In_channel.stdin |> print_triple
