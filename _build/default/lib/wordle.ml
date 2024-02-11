(* GAME LOGIC *)
open Batteries

type feedback =
  | Correct
  | Incorrect
  | IncorrectPosition
  | WrongDuplicate
  | RightDuplicate

type guess_info = { letter : char; print : feedback }
type answer_info = { aletter : char; dupe : bool; pos : int }

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
  BatList.mapi
    (fun pos c ->
      if BatString.count_char answer c = 1 then
        { aletter = c; dupe = false; pos }
      else { aletter = c; dupe = true; pos })
    (make_list answer)

let print_feedback = function
  | Correct -> "Correct."
  | Incorrect -> "Incorrect."
  | IncorrectPosition -> "Wrong Position."
  | WrongDuplicate -> "Wrong Position. Word has duplicates of this letter."
  | RightDuplicate -> "Correct. Word has duplicates of this letter."

(* let print_answer info_list =
   BatList.iter
     (fun info ->
       Printf.printf "Info_List: \nLetter: %c, dupe: %b, Position: %d\n"
         info.aletter info.dupe info.pos)
     info_list *)

let check_through answer guess =
  let answer_list = make_answer_list answer in
  let guess_list = make_list guess in
  BatList.iter2
    (fun ans_info c ->
      if ans_info.dupe = true then
        match ans_info.aletter = c with
        | true -> Printf.printf "%c : %s \n" c (print_feedback RightDuplicate)
        | false ->
            (* add a check that differentiates between incorrect and wrong duplicate *)
            if BatString.contains answer c then
              Printf.printf "%c : %s \n" c (print_feedback WrongDuplicate)
            else Printf.printf "%c : %s \n" c (print_feedback Incorrect)
      else
        match ans_info.aletter = c with
        | true -> Printf.printf "%c : %s \n" c (print_feedback Correct)
        | false ->
            Printf.printf "a.letter : %c %c : %s \n" ans_info.aletter c
              (print_feedback IncorrectPosition))
    answer_list guess_list

(* let print_feedback answer guess =
   let feedback_list =
     BatList.mapi
       (fun pos guess_c -> (pos, assign_feedback answer guess_c pos guess))
       (make_list guess)
   in
   BatList.iter
     (fun (pos, feedback) ->
       Printf.printf "%c : %s at %d\n" feedback.letter feedback.print_statement
         pos)
     feedback_list *)

(*
   let assign_feedback answer guess pos c =
     if String.contains answer c = false then
       { letter = c; print_statement = "Incorrect." }
     else let correct_position = String.index answer c in

       match guess.[correct_position] = c with
       | true -> (
           match pos = correct_position with
           | true -> { letter = c; print_statement = "Correct." }
           | false -> { letter = c; print_statement = "Incorrect Position." })
       | false -> { letter = c; print_statement = "Incorrect Position." } *)

(* let print_feedback answer guess =
   let feedback_list =
     BatList.mapi
       (fun pos c -> (pos, assign_feedback answer guess pos c))
       (make_list guess)
   in BatList.iter (fun (pos, feedback) ->
     Printf.printf "%c : %s at %d\n"
     feedback.letter feedback.print_statement pos)feedback_list *)

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
