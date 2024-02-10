open Batteries

type feedback = { letter: char; print_statement : string }
(* GAME LOGIC *)

(* loads the dictionary of valid words *)
let load_valid_words () =
  BatList.of_enum (BatFile.lines_of "../data/text-list.txt")

let load_valid_guesses () =
  let list1 = load_valid_words () in
  let list2 = BatList.of_enum (BatFile.lines_of "../data/answer-list.txt") in
  BatList.append list1 list2

let () = Random.self_init ()

let random_word =
  let random_number = 1 + Random.int 2315 in
  let valid_words = load_valid_words () in
  BatList.at valid_words random_number

let make_list str =
  let characters = String.to_list str in
  BatList.of_enum (List.enum characters)

let assign_feedback answer guess c =
  if String.contains answer c = false then
    { letter = c; print_statement = "Incorrect." }
  else let correct_position = String.index answer c in

  match guess.[correct_position] = c with
  | true -> { letter = c; print_statement = "Correct."}
  | false -> { letter = c; print_statement = "Incorrect Position."}

let test_feedback answer guess =
  let feedback_list = 
    BatList.map (assign_feedback answer guess) (make_list guess) in 
    BatList.iter (fun feedback -> Printf.printf "%c : %s\n"
      feedback.letter feedback.print_statement) feedback_list

(* [check guess] is true or false depending on whether the user has inputted the correct guess *)
let check answer guess = answer = guess

let lose_prompt word =
  print_string ("You have ran out of lives. The word was " ^ word ^ ". \n")

let validate_length str_lst = BatList.length str_lst = 5

let validate_word user_input =
  let valid_guesses = load_valid_guesses () in
  BatList.mem user_input valid_guesses

let validate user_input =
  let str_lst = make_list user_input in
  validate_length str_lst && validate_word user_input
