(** @author Nicole Lin (njl55) *)

open A2.Wordle

(** [start ()] is what starts the Wordle game *)
let start () =
  (* let secret_word = "speed" in *)
  let secret_word = random_word in
  let () = style_print "\nWelcome to Wordle, a word guessing game!";
    style_print "\nType 'quit' to quit the game.";
    style_print "\nType 'cheat' to view the answer.";
    style_print "\nOtherwise, type your first guess (must be all lowercase).";
    style_print "\nNOTE: You cannot cheat after you start.";
    print_endline "\n> " in
  let user_guess = read_line () in
  match user_guess with
  | "quit" -> ()
  | "cheat" -> prompt_cheat secret_word
  | _ -> first_guess secret_word user_guess

(* START THE GAME ON OPEN *)
let () = start ()
