open A2.Wordle

(* FOR USER INTERFACE: Printing Output or reading output*)

let rec prompt_guess secret_word count =
  (* subtract one count *)
  let count = count - 1 in
  (* base case: no lives left *)
  if count = 0 then lose_prompt secret_word
  else
    let () =
      print_endline ("You have " ^ string_of_int count ^ " guesses.");
      print_endline "Type your guess: \n> "
    in
    
    let user_guess = read_line () in
    if check secret_word user_guess = true then
      print_endline (user_guess ^ " is correct. Good Job, you win!")
    else prompt_guess secret_word count

let first_guess word guess =
  if check word guess = true then
    print_endline (guess ^ " is correct! Good Job, you win.")
  else if check word guess <> true then prompt_guess word 5

let prompt_cheat secret_word =
  let () = print_endline ("The answer is " ^ secret_word) in
  prompt_guess secret_word 6

let start () =
  let secret_word = random_word in
  let () =
    print_endline "\nWelcome to Wordle, a word guessing game!";
    print_endline "\nType 'quit' to quit the game.";
    print_endline "Type 'cheat' to view the answer.";
    print_endline "Otherwise, type your first guess.";
    print_endline "(NOTE: You cannot cheat after you start!) \n> "
  in
  let user_guess = read_line () in
  if user_guess = "quit" then ()
  else if user_guess = "cheat" then prompt_cheat secret_word
  else first_guess secret_word user_guess

(* TEST.. remove later *)
let () = start ()
