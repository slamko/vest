open Sys

let vest_file_cap = "Vestfile"
let vest_file_low = "vestfile"

type valargs = [ `Valargs of string ]
type program = [ `Program of string ]
type entryname = [ `Entryname of string ]
type symtext = Symtext of string

type operror = 
  | Nothing of string
  | VestfileError of string
  | SyntaxError of string 

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
  | ExpectTerminateArgs
  | ExpectProgram
  | ExpectCloseEntry

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

let symval2str = function 
  | `Text text -> sunwrap text 
  | `Separator sep -> sep2str sep
  | `Empty empty -> empty2str empty

let sym2str sym = symval2str sym.value

let entryval2str = function 
  | `Entryname s -> s 
  | `Valargs s -> s
  | `Program s -> s

let entry2str entry = 
  "{ \n" ^ 
  "  Name: " ^ entryval2str entry.name ^ "\n" ^
  "  Valargs: " ^ entryval2str entry.valargs ^ "\n" ^
  "  Program: " ^ entryval2str entry.program ^ 
  "\n}" 

let synsep sep = 
  `Separator (Syntactsep sep)

let unexpectedsymbol_error unexpsym expsym = 
  Error (
    SyntaxError(
      "Unexpected symbol: '" ^ (sym2str unexpsym) ^
      "' on line: " ^ (string_of_int unexpsym.pos.linenum) ^ " ;\n" ^
      "Expected: " ^ expsym ^ "\n" ))

let prerr = function
  | Nothing err -> prerr_string err
  | VestfileError err -> prerr_string err
  | SyntaxError err -> prerr_string err

let synterror err = Error (SyntaxError err)
let syserror err = Error (SyntaxError err)
let nothing err = Error (Nothing err)

let parsechar chanel : (token, operror) result = 
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
      synterror "eror reading the file"

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
  let rec parse vestfilc symbols : (symbol list, operror) result = 
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
  let rec clean symbols puresymbols = 
    match symbols with 
    | h::t -> 
      begin match h.value with 
      | `Text _ -> h :: puresymbols |> clean t
      | `Separator _ -> h :: puresymbols |> clean t
      | `Empty _ -> 
        
        puresymbols |> clean t end
    | [] -> puresymbols in
  Ok (clean symbols [])

let alertentry newentry = function 
  | _::t -> newentry::t
  | [] -> []

let parse_entries symbols : (valentry list, operror) result  = 
  let rec parse expected entries symbols = match symbols with 
    | h::t ->
        begin match expected with
        | ExpectEntry -> begin match h.value with
          | `Text text -> 
            let newentry = { 
              name = text >> (fun s -> `Entryname s); 
              valargs = `Valargs ""; 
              program = `Program "" 
              } in
            parse ExpectColonSeparator (newentry :: entries) t
          | `Separator sep -> begin match sep with 
            | EOF -> Ok entries 
            | _ -> parse ExpectEntry entries t end
          | _ -> parse ExpectEntry entries t end
        | ExpectColonSeparator -> 
          let unmatched = synsep Colon |> symval2str |> unexpectedsymbol_error h in
          begin match h.value with
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Colon -> parse ExpectValargs entries t
              | _ -> unmatched end 
            | _ -> unmatched end
          | _ -> unmatched end
        | ExpectValargs -> 
          let unmatched = "Valgrind args" |> unexpectedsymbol_error h in
          begin match h.value with
          | `Text text -> 
            let lastentry = entries |> List.hd in
            let entrywithargs = { lastentry with valargs = text >> (fun s -> `Valargs s) } in
            let nentries = alertentry entrywithargs entries in
            parse ExpectTerminateArgs nentries t
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Newline -> parse ExpectProgram entries t
              | _ -> unmatched end 
            | _ -> unmatched end
          | _ -> unmatched end
        | ExpectTerminateArgs ->
          let unmatched = synsep Newline |> symval2str |> unexpectedsymbol_error h in
          begin match h.value with
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Newline -> parse ExpectProgram entries t
              | _ -> unmatched end 
            | _ -> unmatched end
          | _ -> unmatched end
        | ExpectProgram -> 
          let unmatched = "Program" |> unexpectedsymbol_error h in
          begin match h.value with
          | `Text text -> 
            let lastentry = entries |> List.hd in
            let entrywithprogram = { lastentry with program = text >> (fun s -> `Program s) } in
            let nentries = alertentry entrywithprogram entries in
            parse ExpectCloseEntry nentries t
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Newline -> parse ExpectProgram entries t
              | _ -> unmatched end 
            | _ -> unmatched end
          | _ -> unmatched end
        | ExpectCloseEntry -> 
          let unmatched = synsep Newline |> symval2str |> unexpectedsymbol_error h in
          begin match h.value with
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Newline -> parse ExpectEntry entries t
              | _ -> unmatched end 
            | _ -> unmatched end
          | _ -> unmatched end end
    | [] -> Ok entries in
  parse ExpectEntry [] symbols

let rec eval_entries valentries = 
    match valentries with 
    | h::t -> 
      begin try 
        command @@ "valgrind " ^ entryval2str h.valargs |> ignore ;
        eval_entries t
      with err -> Printexc.to_string err |> syserror end
    | [] -> Ok ()
  
let check_entries = function 
    | [] -> nothing "No entries specified in Vestfile"
    | entries -> Ok entries

let find_vestfile () = 
  if file_exists vest_file_cap
  then Ok vest_file_cap
  else if file_exists vest_file_low
  then Ok vest_file_low
  else syserror "error: No Vestfile found in the current directory"

let openvestfile vestfile = 
  try 
    let vestfilc = open_in vestfile in
    Ok vestfilc
  with _ -> syserror "error: Can not open Vestfile"

let rec print f = function 
  | [] -> ()
  | h::t -> f h |> print_string ;
  print_char ',' ;
  print_endline "";
  print f t

let (>>=) r f = match r with 
  | Ok value -> f value
  | Error err -> Error err

let () = 
  let result = 
    find_vestfile ()
    >>= openvestfile
    >>= parse_symbols
    >>= clean_emptysymbs 
    >>= parse_entries 
    >>= check_entries
    >>= eval_entries
    (*
    *)
  in
  match result with 
  | Ok value -> value (*|> print entry2str *)
  | Error err ->
    prerr err ;
    exit 1
