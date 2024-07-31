open Alcotest

let test_challenge c expected () =
  check string "same string" (c ()) expected

let suite =
  [ "s1c1", `Quick, test_challenge Cryptocaml.Set1.set1c1 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t"
  ; "s1c2", `Quick, test_challenge Cryptocaml.Set1.set1c2 "746865206b696420646f6e277420706c6179"
  ]

let () =
  Alcotest.run "Cryptopals" [ "Challenges", suite ]
