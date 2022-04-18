open Types

let clean_emptysymbs symbols = 
  let rec clean symbols puresymbols = 
    match symbols with 
    | h::t -> 
      begin match h.value with 
      | `Text _ -> h :: puresymbols |> clean t
      | `Separator _ -> h :: puresymbols |> clean t
      | `Empty _ -> puresymbols |> clean t end
    | [] -> puresymbols in
  Ok (clean symbols [])

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
      vestfilerr "error reading the file"

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

let reached_eof parsedsym = match parsedsym.separator with
  | EOF -> true
  | _ -> false

let parse_symbols vestfilc = 
  let rec parse vestfilc symbols : (symbol list, operror) result = 
    match parsechar vestfilc with 
    | Ok token -> 
      begin match matchtoken token with
        | `Text ch -> begin match read_symbol vestfilc ch with 
            | Ok parsed -> 
              let appended = parsed |> append_parsedsym symbols in
              if reached_eof parsed 
              then Ok appended
              else parse vestfilc appended
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