open Alcotest
open Cryptocaml

let test_challenge c expected () =
  check string "same string" (c ()) expected

let test_hamming_distance a b expected () =
  check int "same number" (Set1.hamming_distance (Bytes.of_string a) (Bytes.of_string b)) expected

let suite =
  [
    "s1c1", `Quick, test_challenge Set1.set1c1 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";
    "s1c2", `Quick, test_challenge Set1.set1c2 "746865206b696420646f6e277420706c6179";
    "s1c5", `Quick, test_challenge Set1.set1c5 "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f";
    "hamming_distance", `Quick, test_hamming_distance "this is a test" "wokka wokka!!!" 37;
  ]

let () =
  Alcotest.run "Cryptopals" [ "Challenges", suite ]
