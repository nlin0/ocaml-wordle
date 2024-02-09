open Batteries

let test x y = x + y
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

(* [check guess] is true or false depending on whether the user has inputted the correct guess *)
let check correct guess = correct = guess

let lose_prompt word =
  print_string ("You have ran out of lives. The word was " ^ word ^ ". \n")

let make_list str = 
  let characters = String.to_list str in
  BatList.of_enum (List.enum characters) 

let validate_length str_lst = 
    BatList.length str_lst = 5
  
let validate_word user_input =  
  let valid_guesses = load_valid_guesses () in 
  BatList.mem user_input valid_guesses

let validate user_input =
  let str_lst = make_list user_input in
  validate_length str_lst && validate_word user_input



(* let check_word user_input word =
  let str_lst = make_list word in
  if BatList.length str_lst <> 5 then 
    print_endline "Invalid word. Your word must be exactly 5 letters long."
  else if BatList.mem user_input str_lst then 
    print_endline "Invalid word. Word was not recognized, try another."
  else (); *)
