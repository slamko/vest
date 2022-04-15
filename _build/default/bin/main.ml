open Sys

let vest_file_cap = "Vestfile"
let vest_file_low = "vestfile"

type valargs = string
type program = string

type token =
  | Space
  | Newline
  | Colon
  | Char of char
  | EOF

type separator = 
  | Colon
  | Newline
  | EOF

type symbol = 
  | Text of string
  | Separator of separator

type rsymbol = {
  symbol: string;
  separator: separator
}

type valentry = { 
  name: string;
  valargs: valargs;
  program: program
}

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
    | _ -> Ok (Char c)
  with 
    End_of_file ->
      close_in chanel ;
      Ok EOF
    | _ -> 
      close_in_noerr chanel ;
      Error "eror reading the file"
  
let matchtoken token psymbol = 
  match token with 
    | Char c -> Text (psymbol ^ (Char.escaped c))
    | Space -> Text (psymbol ^ " ")
    | Newline -> Separator Newline
    | Colon -> Separator Colon
    | EOF -> Separator EOF 

let rec read (psymbol: string) vestfilc : (rsymbol, string) result =  
  match parsechar vestfilc with 
  | Ok token ->  
    begin 
      match token with 
      | Char c -> Ok (read (psymbol ^ (Char.escaped c)) vestfilc)
      | Space -> Ok (read (psymbol ^ " ") vestfilc)
      | Newline -> Ok { symbol = psymbol; separator = Newline }
      | Colon -> Ok { symbol = psymbol; separator = Colon }
      | EOF -> Ok{ symbol = psymbol; separator = EOF } in
    end
  | Error err -> Error err

let read_symbol vestfilc schar = 
  in
  read []

let rec parse_symbols vestfilc = 
  match parsechar vestfilc with 
  | Ok token -> 
  | Error  err -> 

let parse_entries vestfile = 
  let vestfilc = open_in vestfile in
  
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
      match parse_entries vestfile with 
      | Ok e -> print_endline "ok"
      | Error s -> print_endline "ka" 
  | None -> print_endline "No vestfile found in the current directory" ;;

  print_endline ""
