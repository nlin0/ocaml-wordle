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

let print_colored_feedback feedbck c color =
  let str = print_feedback feedbck in

  ANSITerminal.printf
    [ ANSITerminal.Bold; color; ANSITerminal.on_default ]
    "%c: " c;
  ANSITerminal.printf [ color; ANSITerminal.on_white ] "%s " str;
  ANSITerminal.printf [ color; ANSITerminal.on_default ] "\n"

let print_char_feedback feedbck c =
  match feedbck with
  | Correct -> print_colored_feedback feedbck c ANSITerminal.green
  | Incorrect -> print_colored_feedback feedbck c ANSITerminal.red
  | IncorrectPosition -> print_colored_feedback feedbck c ANSITerminal.yellow
  | WrongDuplicate -> print_colored_feedback feedbck c ANSITerminal.cyan
  | RightDuplicate -> print_colored_feedback feedbck c ANSITerminal.green

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
  | true -> print_char_feedback RightDuplicate c
  | false -> print_char_feedback Correct c

let dupe_no_match contains_dupe answer c =
  if BatString.contains answer c then
    match contains_dupe = true with
    | true -> print_char_feedback WrongDuplicate c
    | false -> print_char_feedback IncorrectPosition c
  else print_char_feedback Incorrect c

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
