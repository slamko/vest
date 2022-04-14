open Sys

let vest_file_cap = "Vestfile"
let vest_file_low = "vestfile"

type valentry = { 
  valargs: string;
  program: string
}

type token =
  | Space
  | Newline
  | Colon
  | Char
  | EOF

let error_no_vestfile () =
  prerr_endline "error: No Vestfile found in the current directory"

let execute_valentries valentrylist = 
  0

let parsechar chanel = 
  try 
    let c = input_char chanel in
    match c with 
    | ' ' -> Ok Space
    | '\t' -> Ok Space
    | '\n' -> Ok Newline
    | _ -> Ok Char
  with 
    End_of_file ->
      close_in chanel ;
      Ok EOF
    | _ -> 
      close_in_noerr chanel ;
      Error "eror reading the file"

let parse_entries vestfile = 
  let vestfilc = open_in vestfile in
  (*let rec read_vestfile = 
    *)  
  Error "ERROR"

let find_vestfile () = 
  if file_exists vest_file_cap
  then Some vest_file_cap
  else if file_exists vest_file_low
  then Some vest_file_low
  else None

let () =
  match find_vestfile () with
  | Some vestfile -> 
      let som () = match parse_entries vestfile with 
      | Ok e -> print_endline "ok"
      | Error s -> print_endline "ka" in
      som ()
  | None -> print_endline "No vestfile found in the current directory";;

  print_endline ""
