open Alcotest
open Cryptocaml

let test_challenge c expected () =
  check string "same string" expected (c ())

let test_hamming_distance a b expected () =
  check int "same number" expected
    (Set1.hamming_distance (Bytes.of_string a) (Bytes.of_string b))

let test_pkcs7_pad size block expected () =
  check bytes "same bytes"
    (Bytes.of_string expected)
    (Set2.pkcs7_pad size (Bytes.of_string block))

let test_pkcs7_unpad size text expected () =
  check bytes "same bytes"
    (Bytes.of_string expected)
    (Set2.pkcs7_unpad size (Bytes.of_string text))

let suite =
  [
    "s1c1", `Quick, test_challenge Set1.set1c1 "SSdtIGtpbGxpbmcgeW91ciBicmFpbiBsaWtlIGEgcG9pc29ub3VzIG11c2hyb29t";
    "s1c2", `Quick, test_challenge Set1.set1c2 "746865206b696420646f6e277420706c6179";
    "s1c5", `Quick, test_challenge Set1.set1c5 "0b3637272a2b2e63622c2e69692a23693a2a3c6324202d623d63343c2a26226324272765272a282b2f20430a652e2c652a3124333a653e2b2027630c692b20283165286326302e27282f";

    "hamming_distance", `Quick, test_hamming_distance "this is a test" "wokka wokka!!!" 37;

    "s2c9", `Quick, test_challenge Set2.set2c9 "YELLOW SUBMARINE\x04\x04\x04\x04";

    "pkcs7-three", `Quick, test_pkcs7_pad 20 "YELLOW SUBMARINES" "YELLOW SUBMARINES\x03\x03\x03";
    "pkcs7-none", `Quick, test_pkcs7_pad 16 "YELLOW SUBMARINE" "YELLOW SUBMARINE";
    "pkcs7-multi", `Quick, test_pkcs7_pad 3 "YELLOW SUBMARINE" "YELLOW SUBMARINE\x02\x02";

    "pkcs7-not-padded", `Quick, test_pkcs7_unpad 3 "ABC" "ABC";
    "pkcs7-not-full-block1", `Quick, test_pkcs7_unpad 3 "AB" "AB";
    "pkcs7-not-full-block2", `Quick, test_pkcs7_unpad 3 "A" "A";
    "pkcs7-unpad-1", `Quick, test_pkcs7_unpad 3 "AB\x01" "AB";
    "pkcs7-unpad-2", `Quick, test_pkcs7_unpad 3 "A\x02\x02" "A";
    "pkcs7-short-padding", `Quick, test_pkcs7_unpad 3 "A\x01\x01" "A\x01";
    "pkcs7-bad-padding", `Quick, test_pkcs7_unpad 3 "AB\x02" "AB\x02";
    "pkcs7-unpad-3", `Quick, test_pkcs7_unpad 16 "er\014\014\014\014\014\014\014\014\014\014\014\014\014\014" "er";
    "pkcs7-unpad-more-blocks", `Quick, test_pkcs7_unpad 3 "XYZAB\x01" "XYZAB";
  ]

let () =
  Alcotest.run "Cryptopals" [ "Challenges", suite ]
