let c1data = "49276d206b696c6c696e6720796f757220627261696e206c696b65206120706f69736f6e6f7573206d757368726f6f6d"

let hexdecode s = `Hex s |> Hex.to_bytes
let hexencode b = Hex.of_bytes b |> Hex.show

let b64encode bytes = Bytes.to_string bytes |> Base64.encode_string

let set1c1 () = hexdecode c1data |> b64encode

let c2data1 = "1c0111001f010100061a024b53535009181c"
let c2data2 = "686974207468652062756c6c277320657965"
let set1c2 () = Common.xor_bytes (hexdecode c2data1) (hexdecode c2data2) |> hexencode

let c3data = "1b37373331363f78151b7f2b783431333d78397828372d363c78373e783a393b3736"

let expected_order = Bytes.of_string "etaoin shrdlucmfwypvbgkqjxz"

module CharMap = Map.Make(Char)

let char_frequencies text =
  let counts = CharMap.empty in
  let add_char dict c = CharMap.update c (function None -> Some 1 | Some n -> Some (n + 1)) dict in
  Bytes.fold_left add_char counts text

let filter_bytes b pred =
  Bytes.to_seq b
  |> Seq.filter pred
  |> Bytes.of_seq

let chars_by_freq text =
  let freqs =
    Bytes.map Char.lowercase_ascii text
    |> char_frequencies in
  let sorted_freqs =
    CharMap.bindings freqs
    |> List.sort (fun (_, a) (_, b) -> compare b a) in
  List.to_seq sorted_freqs
  |> Seq.map fst
  |> Bytes.of_seq

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
    Common.xor_bytes text key_bytes in

  let score_key key =
    let decrypted = xor_with key in
    let result = chars_by_freq decrypted |> score in
    {score=result; key=key; output=decrypted} in

  let all_keys = List.init 256 Char.chr in
  let results = List.map score_key all_keys in
  List.fold_left max {score=0; key='\000'; output=Bytes.empty} results

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


let c5data = Bytes.of_string
  "Burning 'em, if you ain't quick and nimble\n\
   I go crazy when I hear a cymbal"

let c5key = Bytes.of_string "ICE"

let repeat_bytes b len =
  let dest = Bytes.create len in
  for i = 0 to (len - 1) do
    i mod (Bytes.length b)
    |> Bytes.get b
    |> Bytes.set dest i
  done;
  dest

let repeating_key_xor key text =
  Bytes.length text |> repeat_bytes key |> Common.xor_bytes text

let set1c5 () =
  repeating_key_xor c5key c5data |> hexencode


(* Let KEYSIZE be the guessed length of the key; try values from 2 to
   (say) 40. *)

(* Write a function to compute the edit distance/Hamming distance
   between two strings. The Hamming distance is just the number of
   differing bits. The distance between: *)

let nibble_popcounts = [|
  0; (* 0000 *)
  1; (* 0001 *)
  1; (* 0010 *)
  2; (* 0011 *)
  1; (* 0100 *)
  2; (* 0101 *)
  2; (* 0110 *)
  3; (* 0111 *)
  1; (* 1000 *)
  2; (* 1001 *)
  2; (* 1010 *)
  3; (* 1011 *)
  2; (* 1100 *)
  3; (* 1101 *)
  3; (* 1110 *)
  4; (* 1111 *)
|]

let popcount c =
  let i = Char.code c in
  let bottom = i land 0x0f in
  let top = i lsr 4 in
  nibble_popcounts.(bottom) + nibble_popcounts.(top)

let hamming_distance a b =
  Common.xor_bytes a b
  |> Bytes.to_seq
  |> Seq.fold_left (fun i c -> i + (popcount c)) 0

(* this is a test *)
(* and *)
(* wokka wokka!!! *)
(* is 37. Make sure your code agrees before you proceed. *)

(* For each KEYSIZE, take the first KEYSIZE worth of bytes, and the
   second KEYSIZE worth of bytes, and find the edit distance between
   them. Normalize this result by dividing by KEYSIZE. *)

(* The KEYSIZE with the smallest normalized edit distance is probably
   the key. You could proceed perhaps with the smallest 2-3 KEYSIZE
   values. Or take 4 KEYSIZE blocks instead of 2 and average the
   distances. *)

(* NOTE: I was curious why this would work, found a good answer here:
   https://crypto.stackexchange.com/a/8118*)

let find_keysize candidates text =
  let normalise size distance =
    (float_of_int distance) /. (float_of_int size)
  in
  let normalised_distance b1 b2 =
    hamming_distance b1 b2 |> normalise (Bytes.length b1)
  in
  let try_keysize size =
    let blocks = Seq.init 4 (fun i -> Bytes.sub text (size*i) size) in
    let pair_distances = Seq.map2 normalised_distance blocks (Seq.drop 1 blocks) in
    (Seq.fold_left (+.) 0. pair_distances) /. (float_of_int (candidates - 1))
  in
  List.init 38 ((+) 2)
  |> List.map (fun n -> (try_keysize n, n))
  |> List.sort compare


(* Now that you probably know the KEYSIZE: break the ciphertext into
   blocks of KEYSIZE length. *)

(* Now transpose the blocks: make a block that is the first byte of
   every block, and a block that is the second byte of every block,
   and so on. *)

let transpose blocks =
  let blocks_seq = List.map Bytes.to_seq blocks |> List.to_seq in
  Seq.transpose blocks_seq |> Seq.map Bytes.of_seq |> List.of_seq


(* Solve each block as if it was single-character XOR. You already
   have code to do this. *)

let solve blocks =
  let scores = List.map decrypt blocks in
  List.fold_left (fun acc {key; _} -> key::acc) [] scores
  |> List.rev
  |> List.to_seq
  |> Bytes.of_seq

(* For each block, the single-byte XOR key that produces the best
   looking histogram is the repeating-key XOR key byte for that
   block. Put them together and you have the key. *)

let set1c6 () =
  let text = Common.input () in
  let key = Common.blocks 29 text |> transpose |> solve in
  repeating_key_xor key text |> Bytes.to_string |> Printf.sprintf "Key: %S\n%s" (Bytes.to_string key)

let load filename =
  In_channel.with_open_text filename In_channel.input_all |> Common.b64decode

let c7key = Bytes.of_string "YELLOW SUBMARINE"

let set1c7 () =
  let text = Common.input () in
  Common.aes_ecb c7key text |> Bytes.to_string

module BytesMap = Map.Make(Bytes)

let find_dupe_blocks size text =
  let blocks = Common.blocks size text in
  let add_block dict block =
    BytesMap.update block (function None -> Some 1 | Some n -> Some  (n + 1)) dict
  in
  let block_map =
    List.fold_left add_block BytesMap.empty blocks
  in
  BytesMap.filter (fun _ v -> v > 1) block_map
  |> BytesMap.to_list

let find_lines_with_dupes lines =
  let check_line i line =
    let dupes =
      Common.b64decode line
      |> find_dupe_blocks 16 in
    (i, dupes)
  in
  List.mapi check_line lines
  |> List.filter (fun (_, dupes) -> (List.length dupes) > 0)


let set1c8 () =
  let lines = In_channel.input_lines In_channel.stdin in
  let dupe_lines = find_lines_with_dupes lines in
  let buf = Buffer.create 0 in
  let print_dupe (b, count) =
    Printf.sprintf "  (%S, %d)\n" (Bytes.to_string b) count
    |> Buffer.add_string buf
  in
  List.iter (
    fun (lno, dupes) ->
      Printf.sprintf "%d: [\n" lno |> Buffer.add_string buf;
      List.iter print_dupe dupes;
      Buffer.add_string buf "]\n"
  ) dupe_lines;
  Buffer.contents buf
