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

type empty = 
  | Space
  | Tab

type clsymbol = 
  | Text of symtext
  | Separator of separator

type symbol = 
  | Text of symtext
  | Empty of empty
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

let parsechar chanel : (token, string) result = 
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
  | Space -> Empty Space
  | Tab -> Empty Tab
  | Newline -> Separator Newline
  | Colon -> Separator Colon
  | EOF -> Separator EOF   

let matchtok_withpr parsedsym token : clsymbol = 
  match token |> matchtoken with 
  | Text text -> Text (parsedsym ^ text)
  | Empty _ -> Text parsedsym
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
  read schr vestfilc

let some fg dg = 
  print_char dg ;
  fg + 5

let acc = some 

let append_parsedsym seq rsymbol = 
  seq @ [Text rsymbol.symbol] @ [Separator rsymbol.separator]

let appendsym vestfilc seq = 
  function 
  | Text ch -> 
    begin 
      match read_symbol vestfilc ch with 
      | Ok parsed -> Ok (parsed |> append_parsedsym seq)
      | Error err -> Error err
    end
  | Empty empty -> Ok (seq @ [Empty empty])
  | Separator separator -> Ok (seq @ [Separator separator])

let parse_symbols vestfilc = 
  let rec parse vestfilc symbols : (symbol list, string) result = 
    match parsechar vestfilc with 
    | Ok token -> 
      begin 
        let symb = matchtoken token |> appendsym vestfilc symbols in
        match symb with 
        | Ok symbs -> parse vestfilc symbs
        | Error err -> Error err
      end 
    | Error err -> Error err in 
  parse vestfilc [] 

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
    begin
      match parse_entries vestfile with 
      | Ok e -> print_endline "ok"
      | Error s -> print_endline "ka" 
    end
  | None -> error_no_vestfile () ;;

  print_endline ""
