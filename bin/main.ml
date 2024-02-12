(** @author Nicole Lin (njl55) *)

open A2.Wordle

(** [style_print string] is the color-ified printed string *)
let style_print string =
  ANSITerminal.print_string [ ANSITerminal.white; ANSITerminal.on_black ] string

(** [lose_prompt word] is the colored printed string informing the player of game over *)
let lose_prompt word =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; ANSITerminal.red ]
    ("You have ran out of lives. The word was " ^ word ^ ". \n")

(** [win_prompt word] is the colored printed string informing the player that they won *)
let win_prompt word =
  ANSITerminal.print_string
    [ ANSITerminal.Bold; ANSITerminal.green ]
    (word ^ " is correct! Good Job, you win! \n")

(** [prompt_guess secrete_word count] is the prompt that asks user for a guess as long as the user has enough guesses [count] *)
let rec prompt_guess secret_word count =
  let count = count - 1 in
  if count = 0 then lose_prompt secret_word
  else
    let () =
      style_print (string_of_int count ^ " guesses left.");
      print_endline "\n>"
    in
    let user_guess = read_line () in
    match validate user_guess with
    | false ->
        let () =
          style_print "Invalid word. Word must be a 5 letter recognized word\n"
        in
        prompt_guess secret_word (count + 1)
    | true ->
        if check secret_word user_guess = true then win_prompt user_guess
        else
          let () = check_through secret_word user_guess in
          prompt_guess secret_word count

(** [prompt_cheat secret_word] is what reveals [secret_word] if the user wants to cheat  *)
let prompt_cheat secret_word =
  let () = style_print ("The answer is " ^ secret_word ^ "\n") in
  prompt_guess secret_word 6

(** [first_guess secret_word user_guess] is the prompt when the user makes their first guess [user_guess] without cheating *)
let first_guess secret_word user_guess =
  match validate user_guess with
  | false ->
      let () =
        style_print "Invalid word. Word must be a 5 letter recognized word.\n"
      in
      prompt_guess secret_word 6
  | true -> (
      match check secret_word user_guess with
      | true -> win_prompt user_guess
      | false ->
          let () = check_through secret_word user_guess in
          prompt_guess secret_word 5)

(** [start ()] is what starts the Wordle game *)
let start () =
  let secret_word = random_word in
  let () =
    style_print "\nWelcome to Wordle, a word guessing game!";
    style_print "\nType 'quit' to quit the game.";
    style_print "\nType 'cheat' to view the answer.";
    style_print "\nOtherwise, type your first guess (must be all lowercase).";
    style_print "\nNOTE: You cannot cheat after you start.";
    print_endline "\n> "
  in
  let user_guess = read_line () in
  match user_guess with
  | "quit" -> ()
  | "cheat" -> prompt_cheat secret_word
  | _ -> first_guess secret_word user_guess

(* START THE GAME ON OPEN *)
let () = start ()
