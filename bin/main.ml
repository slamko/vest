open Sys

let vest_file_cap = "Vestfile"
let vest_file_low = "vestfile"

type valargs = Valargs of string
type program = Program of string
type symtext = Symtext of string

type token =
  | Space
  | Newline
  | Colon
  | Tab
  | Char of char
  | EOF

type syntactsep = 
  | Colon
  | Newline

type separator = 
  | Syntactsep of syntactsep
  | EOF

type empty = 
  | Space
  | Tab

type basesym = [
  | `Text of symtext
  | `Separator of separator]

type symbol = [
  | basesym
  | `Empty of empty ]

type rsymbol = {
  symbol: symtext;
  separator: separator
}

type valentry = { 
  name: string;
  valargs: valargs;
  program: program
}

let error_no_vestfile () =
  prerr_endline "error: No Vestfile found in the current directory"

let evalentries valentrylist = 
  0

let tofsymt symtext = 
  `Text symtext

let synsep sep = 
  `Separator (Syntactsep sep)

let parsechar chanel c : (token, string) result = 
  try 
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
  | Char c -> Symtext (Char.escaped c) |> tofsymt
  | Space -> `Empty Space
  | Tab -> `Empty Tab
  | Newline -> synsep Newline
  | Colon -> synsep Colon
  | EOF -> `Separator EOF   

let (++) symb symapp : symtext = 
  match symb with 
  | Symtext s -> begin match symapp with 
    | Symtext sapp -> Symtext (s ^ sapp) end

let matchtok_withpr parsedsym token = 
  match token |> matchtoken with 
  | `Text text -> parsedsym ++ text |> tofsymt
  | `Empty _ -> tofsymt parsedsym
  | `Separator separator -> `Separator separator


let read_symbol vestfilc schr = 
  let rec read vestfilc psymbol = 
    let rch = input_char vestfilc in 
    match parsechar vestfilc rch with 
    | Ok token ->  
      begin match matchtok_withpr psymbol token  with 
        | `Text text -> read vestfilc text 
        | `Separator sep -> 
          let parsedsym = { symbol = psymbol; separator = sep } in
          Ok parsedsym
      end
    | Error err -> Error err in
  read vestfilc schr 

let append_parsedsym seq rsymbol = 
  seq @ [`Text rsymbol.symbol] @ [`Separator rsymbol.separator]

let parse_symbols vestfilc = 
  let rec parse vestfilc symbols : (symbol list, string) result = 
    let rch = input_char vestfilc in 
    match parsechar vestfilc rch with 
    | Ok token -> 
      begin match matchtoken token with
        | `Text ch ->
          begin match read_symbol vestfilc ch with 
            | Ok parsed -> parsed |> append_parsedsym symbols |> parse vestfilc
            | Error err -> Error err
          end
        | `Empty empty -> symbols @ [`Empty empty] |> parse vestfilc
        | `Separator separator -> 
            begin match separator with 
            | Syntactsep sep -> symbols @ [synsep sep] |> parse vestfilc
            | EOF -> Ok (symbols @ [`Separator EOF])
            end 
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
