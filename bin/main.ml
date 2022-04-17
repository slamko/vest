open Sys

let vest_file_cap = "Vestfile"
let vest_file_low = "vestfile"

type valargs = Valargs of string
type program = Program of string
type symtext = Symtext of string
type entryname = Entryname of string

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

type symval = [
  | basesym
  | `Empty of empty ]

type sympos = {
  linenum: int
}

type symbol = {
  value: symval;
  pos: sympos
}

type parsesym = {
  symbol: symtext;
  separator: separator
}

type valentry = { 
  name: entryname;
  valargs: valargs;
  program: program
}

let error_no_vestfile () =
  prerr_endline "error: No Vestfile found in the current directory"

let evalentries valentrylist = 
  0

let tofsymt symtext = 
  `Text symtext

let sep2str = function 
  | Syntactsep sep -> 
    begin match sep with 
    | Newline -> "New line"
    | Colon -> "Colon" end
  | EOF -> "EOF"

let empty2str = function 
  | Space -> " "
  | Tab -> "\t"

let synsep sep = 
  `Separator (Syntactsep sep)

let parsechar chanel : (token, string) result = 
  try 
    let c = input_char chanel in 
    match c with 
    | ' ' -> Ok Space
    | '\t' -> Ok Tab
    | ':' -> Ok Colon
    | '\n' -> Ok Newline
    | _ -> Ok (Char c)
  with 
    End_of_file ->
      close_in chanel ;
      Ok EOF
    | _ -> 
      close_in_noerr chanel ;
      Error "eror reading the file"

let matchtoken token : symval = 
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

let symappend = (++)

let (>>) s f =
  match s with  
  | Symtext s -> f s

let (<<) s = Symtext s

let tosym = (<<)

let matchtokpref parsedsym token = 
  match token |> matchtoken with 
  | `Text text -> parsedsym ++ text |> tofsymt
  | `Empty emptySym -> emptySym |> empty2str |> tosym |> (++) parsedsym |> tofsymt
  | `Separator separator -> `Separator separator

let read_symbol vestfilc schr = 
  let rec read vestfilc psymbol = 
    match parsechar vestfilc with 
    | Ok token ->  
      begin match matchtokpref psymbol token  with 
        | `Text text -> read vestfilc text 
        | `Separator sep -> 
          let parsedsym = { symbol = psymbol; separator = sep } in
          Ok parsedsym
      end
    | Error err -> Error err in
  read vestfilc schr 

let appendsym symval (seq: symbol list) = 
  match seq with 
  | [] -> 
    let newsym = { value = symval; pos = { linenum = 1 } } in
    newsym :: seq
  | _ -> 
    let lastsym = seq |> List.hd in
    let newsym = { value = symval; pos = { linenum = lastsym.pos.linenum } } in
    begin match lastsym.value with 
    |  `Separator sep -> 
      begin match sep with 
      | Syntactsep sep -> 
        begin match sep with 
        | Newline -> { newsym with pos = { linenum = lastsym.pos.linenum + 1 } } :: seq
        | _ -> newsym :: seq end
      | _ -> newsym :: seq end
    | _ -> newsym :: seq end

let append_parsedsym seq parsedsym = 
  seq |> appendsym @@ `Text parsedsym.symbol |> appendsym @@ `Separator parsedsym.separator 

let parse_symbols vestfilc = 
  let rec parse vestfilc symbols : (symbol list, string) result = 
    match parsechar vestfilc with 
    | Ok token -> 
      begin match matchtoken token with
        | `Text ch -> begin match read_symbol vestfilc ch with 
            | Ok parsed -> parsed |> append_parsedsym symbols |> parse vestfilc
            | Error err -> Error err
          end
        | `Empty empty -> symbols |> appendsym @@ `Empty empty |> parse vestfilc
        | `Separator separator -> 
          begin match separator with 
          | EOF -> Ok ( symbols |> appendsym @@ `Separator EOF)
          | _ -> symbols |> appendsym @@ `Separator separator |> parse vestfilc
          end 
      end 
    | Error err -> Error err in 
  parse vestfilc [] 

let clean_emptysymbs symbols = 
  let rec clean puresymbols = function 
    | h::t -> 
      begin match h.value with 
      | `Text _ -> h :: puresymbols |> clean t
      | `Separator _ -> h :: puresymbols |> clean t
      | `Empty _ -> clean t puresymbols end
    | [] -> puresymbols in
  clean symbols []

let parse_entries symbols = 
  let rec parse entries = function
    | 
  parse symbols [] 

let find_vestfile () = 
  if file_exists vest_file_cap
  then Some vest_file_cap
  else if file_exists vest_file_low
  then Some vest_file_low
  else None

let rec print_symbols = function 
  | [] -> ()
  | h::t -> 
    begin match h with 
    | `Text text -> 
      text >> print_string ;
      | `Empty empty -> empty |> empty2str |> print_string
      | `Separator sep -> sep |> sep2str |> print_string end ;
  print_char ',' ;
  print_endline "";
  print_symbols t

let print symbols = symbols |> List.rev |> print_symbols

let runflow vestfile = 
  let source = open_in vestfile in
  match parse_symbols source with 
  | Ok symbols -> 
    symbols |> print ;
    symbols |> clean_emptysymbs |> List.rev ; ()
  | Error err -> prerr_string err

let () =
  match find_vestfile () with
  | Some vestfile -> runflow vestfile
  | None -> error_no_vestfile ()
