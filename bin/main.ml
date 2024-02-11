(** @author Nicole Lin (njl55) *)

open A2.Wordle

(* FOR USER INTERFACE: Printing Output or reading output*)
let style_print string =
  ANSITerminal.print_string [ ANSITerminal.black; ANSITerminal.on_white ] string

let rec prompt_guess secret_word count =
  let count = count - 1 in
  if count = 0 then lose_prompt secret_word else let () =
    print_endline ("Wrong. " ^ string_of_int count ^ " guesses left. \n>") in
    let user_guess = read_line () in
    match validate user_guess with
    | false -> let () = print_endline 
        "Invalid word. Word must be a 5 letter recognized word" in
        prompt_guess secret_word (count + 1)
    | true -> if check secret_word user_guess = true then
          print_endline (user_guess ^ " is correct. Good Job, you win!")
        else let () = check_through secret_word user_guess in
          prompt_guess secret_word count

let first_guess secret_word user_guess =
  match validate user_guess with
  | false -> let () = print_endline 
      "Invalid word. Word must be a 5 letter recognized word."in
      prompt_guess secret_word 6
  | true -> ( match check secret_word user_guess with
      | true -> ANSITerminal.print_string 
        [ ANSITerminal.white; ANSITerminal.on_green ] 
        (user_guess ^ " is correct! Good Job, you win.")
      | false -> let () = check_through secret_word user_guess in
          prompt_guess secret_word 5)

let prompt_cheat secret_word =
  let () = print_endline ("The answer is " ^ secret_word) in
  prompt_guess secret_word 6

let start () =
  (* let secret_word = "speed" in *)
  let secret_word = random_word in
  let () =
    style_print "\nWelcome to Wordle, a word guessing game!";
    style_print "\nType 'quit' to quit the game.";
    style_print "\nType 'cheat' to view the answer.";
    style_print "\nOtherwise, type your first guess (must be all lowercase).";
    style_print "\nNOTE: You cannot cheat after you start.";
    print_endline "\n> "
  in let user_guess = read_line () in
  match user_guess with
  | "quit" -> ()
  | "cheat" -> prompt_cheat secret_word
  | _ -> first_guess secret_word user_guess

(* TEST.. remove later *)
let () = start ()
