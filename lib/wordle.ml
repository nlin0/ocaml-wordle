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

(** [print_feedback] is the string of the string feedback of each character *)
let print_feedback = function
  | Correct -> "Correct."
  | Incorrect -> "Incorrect."
  | IncorrectPosition -> "Letter Exists."
  | WrongDuplicate -> "Letter Exists. Word has duplicates of this letter."
  | RightDuplicate -> "Correct. Word has duplicates of this letter."

(** [print_colored_feedback feedbck c color] is [c] and [feedbck] formatted 
    and printed with color *)
let print_colored_feedback feedbck c color =
  let str = print_feedback feedbck in
  ANSITerminal.printf
    [ ANSITerminal.Bold; color; ANSITerminal.on_default ] "%c: " c;
  ANSITerminal.printf [ color; ANSITerminal.on_default ] "%s " str;
  ANSITerminal.printf [ color; ANSITerminal.on_default ] "\n"

(** [color_feedback feedbck c] is colors [c] and [feedbck] green/red/yellow/magenta/cyan depending on [feedbck]*)
let color_feedback feedbck c =
  match feedbck with
  | Correct -> print_colored_feedback feedbck c ANSITerminal.green
  | Incorrect -> print_colored_feedback feedbck c ANSITerminal.red
  | IncorrectPosition -> print_colored_feedback feedbck c ANSITerminal.yellow
  | WrongDuplicate -> print_colored_feedback feedbck c ANSITerminal.magenta
  | RightDuplicate -> print_colored_feedback feedbck c ANSITerminal.cyan

(** [load_valid_words ()] is the string list of guessable words loaded from a .txt file*)
let load_valid_words () =
  BatList.of_enum (BatFile.lines_of "../data/text-list.txt")

(** [load_valid_guesses ()] is the string list of valid input words loaded from the text *)
let load_valid_guesses () =
  let list1 = load_valid_words () in
  let list2 = BatList.of_enum (BatFile.lines_of "../data/answer-list.txt") in
  BatList.append list1 list2

(** [random_word] is a random word [string] picked from a list of valid guessable words*)
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

(** [check answer guess] is true when [answer] is the same as [guess], false otherwise *)
let check answer guess = answer = guess

(** [lose_prompt word] is the printed string informing the player of game over *)
let lose_prompt word =
  print_string ("You have ran out of lives. The word was " ^ word ^ ". \n")

(** [validate user_input] is a true if [user_input] is a valid input for the wordle game, false otherwise *)
let validate user_input =
  let valid_guesses = load_valid_guesses () in
  let str_lst = make_list user_input in
  BatList.length str_lst = 5 && BatList.mem user_input valid_guesses

(** [make_answer_list answer] is a list of records containing information of all 
    the characters in [answer] *)
let make_answer_list answer =
  BatList.map
    (fun c ->
      if BatString.count_char answer c = 1 then { aletter = c; dupe = false }
      else { aletter = c; dupe = true })
    (make_list answer)

(** [make_guess_list answer guess] is a list of records containing information
    of all the characters in [guess] *)
let make_guess_list answer guess =
  BatList.map
    (fun c ->
      if BatString.count_char answer c = 1 then { letter = c; dupe = false }
      else { letter = c; dupe = true })
    (make_list guess)

(** [dupe_match contains_dupe c] is for obtaining feedback for when the letter is a duplicate and in the correct position *)
let dupe_match contains_dupe c =
  match contains_dupe with
  | true -> color_feedback RightDuplicate c
  | false -> color_feedback Correct c

(** [dupe_no_match contains_dupe c] is for obtaining feedback for when the letter is a duplicate and in the incorrect position *)
let dupe_no_match contains_dupe answer c =
  if BatString.contains answer c then
    match contains_dupe = true with
    | true -> color_feedback WrongDuplicate c
    | false -> color_feedback IncorrectPosition c
  else color_feedback Incorrect c

(** [check_through answer guess] is the comparasion of each letter between answer and guess *)
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
