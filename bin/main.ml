let challenges = [
  ("s1c1", Cryptocaml.Set1.set1c1);
  ("s1c2", Cryptocaml.Set1.set1c2);
  ("s1c3", Cryptocaml.Set1.set1c3);
  ("s1c4", Cryptocaml.Set1.set1c4);
  ("s1c5", Cryptocaml.Set1.set1c5);
  ("s1c6", Cryptocaml.Set1.set1c6);
  ("s1c7", Cryptocaml.Set1.set1c7);
  ("s1c8", Cryptocaml.Set1.set1c8);

  ("s2c9", Cryptocaml.Set2.set2c9);
  ("s2c10", Cryptocaml.Set2.set2c10);
  ("s2c11", Cryptocaml.Set2.set2c11);
]

let last_challenge = snd (List.hd (List.rev challenges))

let find_challenge name = List.assoc name challenges

let () =
   let challenge = if (Array.length Sys.argv) > 1 then (find_challenge Sys.argv.(1)) else last_challenge in
   Printf.printf "%s\n" (challenge ())
