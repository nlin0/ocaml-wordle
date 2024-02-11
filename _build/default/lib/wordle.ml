open Batteries

type feedback = { letter : char; print_statement : string }
type letter_info = { aletter : char; checked : bool; pos : int }
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

let make_info_list answer =
    BatList.mapi (fun pos c -> {aletter = c ; checked = false ; pos = pos})
    (make_list answer) 

(* let check_letters info_list c =
  if (info_list.letter = c && info_list.checked = false) then 
    { letter = c; print_statement = "Correct." }
  else if (info_list.letter = c && info_list.checked = true) then 
    { letter = c; print_statement = "Test1." }
  else { letter = c; print_statement = "Test2." } *)

let rec check_letters info_list c = 
  match info_list with
  | [] -> { letter = c; print_statement = "Incorrect." }
  | h :: t ->
    if h.aletter = c then
      let () = print_string ("h.aletter: " ^ String.make 1 h.aletter ^ " char: " ^ String.make 1 c ^ "\n") in
      if h.checked = false then
        let () = print_string ("h.checked is false. Correct \n") in
        { letter = c; print_statement = "Correct." }
      else 
        let () = print_string ("h.checked is true. Incorrect Pos \n") in
        { letter = c; print_statement = "Incorrect Position." }
    else check_letters t c
      

let assign_feedback answer c = 
  if String.contains answer c = false then
    { letter = c; print_statement = "Incorrect." }
  else let answer_info = make_info_list answer in
    check_letters answer_info c

let test_feedback answer guess = 
  let feedback_list =
      BatList.mapi
        (fun pos guess_c -> (pos, assign_feedback answer guess_c))
        (make_list guess)
    in BatList.iter (fun (pos, feedback) ->
      Printf.printf "%c : %s at %d\n" 
      feedback.letter feedback.print_statement pos)feedback_list

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

(* let test_feedback answer guess =
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
