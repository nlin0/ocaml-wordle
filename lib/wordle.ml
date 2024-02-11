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

let print_answer info_list = 
  BatList.iter (fun info ->
    Printf.printf "Info_List: \nLetter: %c, Checked: %b, Position: %d\n"
      info.aletter info.checked info.pos)
    info_list

let test_answer answer = 
  print_answer (make_info_list answer)

let check_position c pos string guess_pos =
  let correct_position = 
    BatString.find_from c pos string in
  correct_position = guess_pos

let rec check_letters info_list answer c pos = 
  match info_list with
  | [] -> { letter = c; print_statement = "Incorrect." }
  | h :: t ->
    let () = Printf.printf "character: %s \n" (String.make 1 c) in
    if h.aletter = c then
      let () = print_string ("h.aletter: " ^ String.make 1 h.aletter ^ " char: " ^ String.make 1 c ^ "\n") in
      if (check_position (String.make 1 c) h.pos answer pos) then
        let () = print_string ("checked is false. Correct \n") in
        { letter = c; print_statement = "Correct." }
      else 
        let () = print_string ("h.checked is true. Incorrect Pos \n") in
        { letter = c; print_statement = "Incorrect Position." }
    else check_letters t answer c pos

      

let assign_feedback answer c pos= 
  if String.contains answer c = false then
    { letter = c; print_statement = "Incorrect." }
  else let answer_info = make_info_list answer in
    check_letters answer_info answer c pos

  
let print_feedback answer guess = 
  let feedback_list =
      BatList.mapi
        (fun pos guess_c -> (pos, assign_feedback answer guess_c pos))
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
