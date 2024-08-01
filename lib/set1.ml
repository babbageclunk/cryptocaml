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

let is_alpha c = (c >= 'a' && c <= 'z') || (c >= 'A' && c <= 'Z')

let expected_order = "etaoinshrdlucmfwypvbgkqjxz"

module CharMap = Map.Make(Char)

let char_frequencies text =
  let counts = CharMap.empty in
  let add_char map c = CharMap.update c (function None -> Some 1 | Some n -> Some (n + 1)) map in
  Bytes.fold_left add_char counts text

let filter_bytes b pred = Bytes.to_seq b |> Seq.filter pred |> Bytes.of_seq

let chars_by_freq text =
  let filtered = filter_bytes text is_alpha |> Bytes.map Char.lowercase_ascii in
  let freqs = char_frequencies filtered in
  let sorted_freqs = CharMap.bindings freqs |> List.sort (fun (_, a) (_, b) -> compare b a) in
  List.to_seq sorted_freqs |> Seq.map fst |> Bytes.of_seq

let set1c3 () = "not done"

(* let set1c3 () = *)
(*   let data = hexdecode c3data in *)

(*   let rec find_key key = *)
(*     let decoded = Bytes.map (fun c -> xor_char c key) data in *)
(*     let score = Bytes.fold_left (fun acc c -> acc + (if is_alpha c then 1 else 0)) 0 decoded in *)
(*     if score > 0 then (key, decoded) else find_key (Char.chr (Char.code key + 1)) in *)
(*   let print_key (key, decoded) = Printf.sprintf "Key: %c\nDecoded: %s" key (Bytes.to_string decoded) in *)
(*   print_key (find_key 'a') *)
