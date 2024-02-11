(** @author Nicole Lin (njl55) *)

open Batteries

type guess_info = { letter : char; dupe : bool }
type answer_info = { aletter : char; dupe : bool }

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


  
(** [load_valid_words ()] is the string list of guessable words loaded from 
    a .txt file*)
let load_valid_words () =
  BatList.of_enum (BatFile.lines_of "../data/text-list.txt")

(** [load_valid_guesses ()] is the string list of valid input words 
    loaded from the text *)
let load_valid_guesses () =
  let list1 = load_valid_words () in
  let list2 = BatList.of_enum (BatFile.lines_of "../data/answer-list.txt") in
  BatList.append list1 list2

(** [random_word] is a random word [string] picked from a list of valid 
    guessable words*)
let random_word =
  let () = Random.self_init () in
  let random_number = 1 + Random.int 2315 in
  let valid_words = load_valid_words () in
  BatList.at valid_words random_number

(* HELPER FUNCTIONS *)
(** [make_list str] is a list consisting of characters of [str] *)
let make_list str =
  let characters = String.to_list str in
  BatList.of_enum (List.enum characters)

(** [check answer guess] is true when [answer] is the same as [guess], false
    otherwise *)
let check answer guess = 
  answer = guess

let lose_prompt word =
  print_string ("You have ran out of lives. The word was " ^ word ^ ". \n")

let validate_length str_lst = BatList.length str_lst = 5

let validate_word user_input =
  let valid_guesses = load_valid_guesses () in
  BatList.mem user_input valid_guesses

let validate user_input =
  let str_lst = make_list user_input in
  validate_length str_lst && validate_word user_input

let make_answer_list answer =
  BatList.map
    (fun c ->
      if BatString.count_char answer c = 1 then { aletter = c; dupe = false }
      else { aletter = c; dupe = true })
    (make_list answer)

let make_guess_list answer guess =
  BatList.map
    (fun c ->
      if BatString.count_char answer c = 1 then { letter = c; dupe = false }
      else { letter = c; dupe = true })
    (make_list guess)

let dupe_match contains_dupe c =
  match contains_dupe with
  | true -> Printf.printf "%c : %s \n" c (print_feedback RightDuplicate)
  | false -> Printf.printf "%c : %s \n" c (print_feedback Correct)

let dupe_no_match contains_dupe answer c =
  if BatString.contains answer c then
    match contains_dupe = true with
    | true -> Printf.printf "%c : %s \n" c (print_feedback WrongDuplicate)
    | false -> Printf.printf "%c : %s \n" c (print_feedback IncorrectPosition)
  else Printf.printf "%c : %s \n" c (print_feedback Incorrect)

let check_through answer guess =
  let answer_list = make_answer_list answer in
  let guess_list = make_guess_list answer guess in
  BatList.iter2
    (fun ans_info guess_info ->
      let c = guess_info.letter in
      match ans_info.aletter = guess_info.letter with
      | true -> dupe_match guess_info.dupe c
      | false -> dupe_no_match guess_info.dupe answer c)
    answer_list guess_list
