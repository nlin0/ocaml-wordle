(* GAME LOGIC *)
open Batteries

type guess_info = { letter : char; dupe: bool }
type answer_info = { aletter : char; dupe : bool}
type feedback =
  | Correct
  | Incorrect
  | IncorrectPosition
  | WrongDuplicate
  | RightDuplicate

let print_feedback = function
  | Correct -> "Correct."
  | Incorrect -> "Incorrect."
  | IncorrectPosition -> "Wrong Position."
  | WrongDuplicate -> "Wrong Position. Word has duplicates of this letter."
  | RightDuplicate -> "Correct. Word has duplicates of this letter."

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

let make_answer_list answer =
  BatList.map (fun c ->
    if BatString.count_char answer c = 1 then
      { aletter = c; dupe = false}
    else { aletter = c; dupe = true}) (make_list answer)

let make_guess_list answer guess = 
  BatList.map (fun c ->
    if BatString.count_char answer c = 1 then { letter = c; dupe = false }
    else { letter = c; dupe = true }) (make_list guess)



let check_through answer guess =
  let answer_list = make_answer_list answer in
  let guess_list = make_guess_list answer guess in
  BatList.iter2 ( fun ans_info guess_info ->
    let c = guess_info.letter in
    match ans_info.aletter = guess_info.letter with
    | true -> ( match guess_info.dupe with
        | true -> Printf.printf "%c : %s \n" c (print_feedback RightDuplicate)
        | false -> Printf.printf "%c : %s \n" c (print_feedback Correct))
    | false -> ( 
      if BatString.contains answer c then match guess_info.dupe = true with 
        |true -> Printf.printf "%c : %s \n" c (print_feedback WrongDuplicate)
        |false -> Printf.printf "%c : %s \n" c (print_feedback IncorrectPosition)
      else Printf.printf "%c : %s \n" c (print_feedback Incorrect)
    )) answer_list guess_list

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
