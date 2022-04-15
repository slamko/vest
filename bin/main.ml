open Sys

let vest_file_cap = "Vestfile"
let vest_file_low = "vestfile"

type valargs = string
type program = string
type symtext = string

type token =
  | Space
  | Newline
  | Colon
  | Tab
  | Char of char
  | EOF

type separator = 
  | Colon
  | Newline
  | EOF

type symbol = 
  | Text of symtext
  | Empty of string
  | Separator of separator

type clsymbol = 
  | Text of symtext
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

let matchtoken token : symbol = 
  match token with
  | Char c -> Text (Char.escaped c)
  | Space -> Empty " "
  | Tab -> Empty "\t"
  | Newline -> Separator Newline
  | Colon -> Separator Colon
  | EOF -> Separator EOF   

let matchclrtok token = 
  match token |> matchtoken with 
  | Text text -> Text text
  | Empty text -> Text text
  | Separator separator -> Separator separator

let matchtok_withpr parsedsym token  = 
  match token |> matchclrtok with 
  | Text text -> Text (parsedsym ^ text)
  | Separator separator -> Separator separator

let read_symbol vestfilc schr = 
  let rec read psymbol vestfilc =  
    match parsechar vestfilc with 
    | Ok token ->  
      begin
        match matchtok_withpr psymbol token  with 
        | Text text -> read text vestfilc
        | Separator sep -> 
          let parsedsym = { symbol = psymbol; separator = sep } in
          Ok parsedsym
        end
        | Error err -> Error err in
  let startsym = Char.escaped schr in
  read startsym vestfilc


let parse_symbols vestfilc = 
  let rec parse vestfilc = 
    match parsechar vestfilc with 
    | Ok token -> 
      begin 
        match matchclrtok token with 
        | Text text ->
        | Separator separator -> 
      end 
    | Error err -> Error err in 
  parse vestfilc    

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
