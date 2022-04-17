open Sys

let vest_file_cap = "Vestfile"
let vest_file_low = "vestfile"

type valargs = Valargs of string
type program = Program of string
type symtext = Symtext of string
type entryname = Entryname of string
type syntaxerror = SyntaxError of string

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

type parsestate = 
  | ExpectEntry
  | ExpectColonSeparator
  | ExpectValargs
  | ExpectNewline
  | ExpectProgram

let error_no_vestfile () =
  prerr_endline "error: No Vestfile found in the current directory"

let evalentries valentrylist = 
  0

let (++) symb symapp : symtext = 
  match symb with 
  | Symtext s -> begin match symapp with 
    | Symtext sapp -> Symtext (s ^ sapp) end

let symappend = (++)

let sunwrap = function 
  | Symtext s -> s

let (>>) s f = sunwrap s |> f 

let (<<) s = Symtext s

let tosym = (<<)

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

let sym2str = function 
  | `Text text -> sunwrap text 
  | `Separator sep -> sep2str sep
  | `Empty empty -> empty2str empty

let synsep sep = 
  `Separator (Syntactsep sep)

let unexpectedsymbol_error unexpsym expsym = 
  Error (
    SyntaxError(
      "Unexpected symbol: " ^ (sym2str unexpsym.value) ^
      " at line: " ^ (string_of_int unexpsym.pos.linenum) ^ ".\n" ^
      "Epected symbol: " ^ expsym ))

let prerr = function
  | SyntaxError s -> prerr_string s

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
      | `Empty _ -> puresymbols |> clean t end
    | [] -> puresymbols in
  clean symbols []

let alertentry newentry = function 
  | _::t -> newentry::t
  | [] -> []

let parse_entries symbols : (valentry list, syntaxerror) result  = 
  let rec parse expected entries symbols = match symbols with 
    | h::t ->
        begin match expected with
        | ExpectEntry -> begin match h.value with
          | `Text text -> 
            let newentry = { 
              name = text >> (fun s -> Entryname s); 
              valargs = Valargs ""; 
              program = Program "" 
              } in
            parse ExpectColonSeparator (newentry :: entries) t
          | `Separator sep -> begin match sep with 
            | EOF -> Ok entries 
            | _ -> parse ExpectEntry entries t end
          | _ -> parse ExpectEntry entries t end
        | ExpectColonSeparator -> 
          let unmatched = synsep Colon |> sym2str |>  unexpectedsymbol_error h in
          begin match h.value with
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Colon -> parse ExpectValargs entries t
              | _ -> unmatched end 
            | _ -> unmatched end
          | _ -> unmatched end
        | ExpectValargs -> 
          let unmatched = "Valgrind args" |>  unexpectedsymbol_error h in
          begin match h.value with
          | `Text text -> 
            let lastentry = entries |> List.hd in
            let entrywithargs = { lastentry with valargs = text >> (fun s -> Valargs s) } in
            let nentries = alertentry entrywithargs entries in
            parse ExpectColonSeparator nentries t
          | _ -> "Valgrind args" |> unexpectedsymbol_error h end
        | ExpectNewline ->
          let unmatched = synsep Newline |> sym2str |>  unexpectedsymbol_error h in
          begin match h.value with
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Newline -> parse ExpectNewline entries t
              | _ -> unmatched end 
            | _ -> unmatched end
          | _ -> unmatched end
        | ExpectProgram -> 
          let unmatched = "Program" |>  unexpectedsymbol_error h in
          begin match h.value with
          | `Text text -> 
            let lastentry = entries |> List.hd in
            let entrywithprogram = { lastentry with program = text >> (fun s -> Program s) } in
            let nentries = alertentry entrywithprogram entries in
            parse ExpectNewline nentries t
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Newline -> parse ExpectProgram entries t
              | _ -> unmatched end 
            | _ -> unmatched end
          | _ -> unmatched end end
    | [] -> Ok (entries) in
  parse ExpectEntry [] symbols

let find_vestfile () = 
  if file_exists vest_file_cap
  then Some vest_file_cap
  else if file_exists vest_file_low
  then Some vest_file_low
  else None

let rec print_symbols = function 
  | [] -> ()
  | h::t -> sym2str h.value |> print_string ;
  print_char ',' ;
  print_endline "";
  print_symbols t

let print symbols = symbols |> List.rev |> print_symbols

let runflow symbols = 
  let result = 
    symbols 
    |> clean_emptysymbs 
    |> List.rev 
    |> parse_entries 
  in
  match result with 
  | Ok _ -> ()
  | Error err ->
    prerr err ;
    exit 1

let main vestfile = 
  let source = open_in vestfile in
  match parse_symbols source with 
  | Ok symbols -> 
    symbols |> print ;
    runflow symbols
  | Error err -> 
      prerr_string err ;
      exit 1

let () =
  match find_vestfile () with
  | Some vestfile -> main vestfile
  | None -> error_no_vestfile ()
