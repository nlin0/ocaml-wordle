(** @author Nicole Lin (njl55) *)

open Batteries

type guess_info = { letter : char; dupe : bool }

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
    [ ANSITerminal.Bold; color; ANSITerminal.on_default ]
    "%c: " c;
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
  let load_valid_words () =
    BatList.of_enum (BatFile.lines_of "../data/text-list.txt") in
  let total_num = List.length (load_valid_words ()) in
  let random_number = 1 + Random.int total_num in
  let valid_words = load_valid_words () in
  BatList.at valid_words random_number

(** [make_list str] is a list consisting of characters of [str] *)
let make_list str =
  let characters = String.to_list str in
  BatList.of_enum (List.enum characters)

(* TESTS *)
let%test "empty string" = make_list "" = []
let%test "non-empty string" = make_list "hello" = [ 'h'; 'e'; 'l'; 'l'; 'o' ]

(** [check answer guess] is true when [answer] is the same as [guess], false otherwise *)
let check answer guess = answer = guess

(* TESTS *)
let%test "same word" = check "yacht" "yacht" = true
let%test "different word" = check "human" "speed" = false
let%test "empty string" = check "" "" = true

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

(** [style_print string] is the color-ified printed string *)
let style_print string =
  ANSITerminal.print_string [ ANSITerminal.white; ANSITerminal.on_black ] string

(** [validate user_input] is a true if [user_input] is a valid input for the wordle game, false otherwise *)
let validate user_input =
  let valid_guesses = load_valid_guesses () in
  let str_lst = make_list user_input in
  BatList.length str_lst = 5 && BatList.mem user_input valid_guesses

(* TESTS *)
let%test "too short" = validate "two" = false
let%test "too long" = validate "orange" = false
let%test "empty string" = validate "" = false
let%test "not recognized" = validate "12345" = false
let%test "valid" = validate "human" = true

(** [make_guess_list answer guess] is a list of records containing information of all the characters in [guess] *)
let make_guess_list answer guess =
  BatList.map
    (fun c ->
      if BatString.count_char answer c = 1 then { letter = c; dupe = false }
      else { letter = c; dupe = true })
    (make_list guess)

(* TESTS *)
let%test "has dupe letters" =
  let expected = make_guess_list "speed" "speed" in
  let result =
    [
      { letter = 's'; dupe = false };
      { letter = 'p'; dupe = false };
      { letter = 'e'; dupe = true };
      { letter = 'e'; dupe = true };
      { letter = 'd'; dupe = false };
    ]
  in
  List.for_all2 (fun expected result -> expected = result) expected result

let%test "has dupe letters" =
  let expected = make_guess_list "thing" "thing" in
  let result =
    [
      { letter = 't'; dupe = false };
      { letter = 'h'; dupe = false };
      { letter = 'i'; dupe = false };
      { letter = 'n'; dupe = false };
      { letter = 'g'; dupe = false };
    ]
  in
  List.for_all2 (fun expected result -> expected = result) expected result

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
  let answer_list = make_list answer in
  let guess_list = make_guess_list answer guess in
  BatList.iter2
    (fun ans_c guess_info ->
      let c = guess_info.letter in
      match ans_c = guess_info.letter with
      | true -> dupe_match guess_info.dupe c
      | false -> dupe_no_match guess_info.dupe answer c)
    answer_list guess_list

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

(** [prompt_cheat secret_word] is what reveals [secret_word] if the user wants to cheat  *)
let prompt_cheat secret_word =
  let () = style_print ("The answer is " ^ secret_word ^ "\n") in
  prompt_guess secret_word 6
