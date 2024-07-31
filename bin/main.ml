let challenges = [
  ("s1c1", Cryptocaml.Set1.set1c1);
  ("s1c2", Cryptocaml.Set1.set1c2);
  ("s1c3", Cryptocaml.Set1.set1c3)
]

let last_challenge = snd (List.hd (List.rev challenges))

let find_challenge name = List.assoc name challenges

let () =
   let challenge = if (Array.length Sys.argv) > 1 then (find_challenge Sys.argv.(1)) else last_challenge in
   Printf.printf "%s\n" (challenge ())
