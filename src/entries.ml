open Types

let rec parse_valout valoutc = 
  try
    let valline = input_line valoutc in
    print_string valline ;
    parse_valout valoutc
  with 
    | End_of_file -> 
      Unix.close_process_in valoutc |> ignore ;
      Ok ()
    | _ -> 
      Unix.close_process_in valoutc |> ignore ;
      syserror "error: Failed to handle Valgrind output"

let rec eval_entries valentries = 
  match valentries with 
  | h::t -> 
    begin try 
      print_endline "" ;
      print_endline "" ;
      print_endline "| | | | | | | | | | | " ;
      print_string @@ entryval2str h.name ;
      print_endline "" ;
      print_endline "| | | | | | | | | | | " ;
      print_endline "" ;
      let valargs = "valgrind " ^ entryval2str h.valargs ^ " " ^ entryval2str h.program in
      let valresult = 
        Unix.open_process_in valargs |> parse_valout in
      match valresult with 
      | Ok _ -> eval_entries t
      | Error err -> Error err
    with err -> Printexc.to_string err |> syserror end
  | [] -> Ok ()
  
let reventries e = Ok (List.rev e)

let check_entries = function 
  | [] -> nothing "No entries found in Vestfile"
  | entries -> Ok entries

let alertentry newentry = function 
  | _::t -> newentry::t
  | [] -> []
  
let preventry = function 
  | h::_ -> h
  | [] -> { 
      name = `Entryname ""; 
      valargs = `Valargs ""; 
      program = `Program "" 
    }

let newentry text = { 
  name = text >> (fun s -> `Entryname s); 
  valargs = `Valargs ""; 
  program = `Program "" 
  }

let swap_lastentry entries lastentry = 
  let valargs = entryval2str lastentry.valargs in
  let entrywithargs = { lastentry with program = `Program valargs; valargs = `Valargs "" } in
  alertentry entrywithargs entries
  
let parse_entries symbols : (valentry list, operror) result  = 
  let rec parse entries symbols expected = match symbols with 
    | h::t ->
        let lastentry = entries |> preventry in
        let parsedefault = parse entries t in
        let parse_rec = parsedefault expected in
        begin match expected with
        | ExpectEntry -> begin match h.value with
          | `Text text -> 
            let newvalentry = newentry text in
            parse (newvalentry :: entries) t ExpectColonSeparator
          | `Separator sep -> begin match sep with 
            | EOF -> Ok entries 
            | _ -> parse_rec end
          | `Empty _ -> unexpectedsymbol_error h "Entry name" end
        | ExpectColonSeparator -> 
          let unmatched = synsep Colon |> symval2readable |> unexpectedsymbol_error h in
          begin match h.value with
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Colon -> parse entries t ExpectValargs
              | _ -> unmatched end 
            | _ -> unmatched end
          | `Empty _ -> parse_rec
          | _ -> unmatched end
        | ExpectValargs -> 
          let unmatched = "Valgrind args" |> unexpectedsymbol_error h in
          begin match h.value with
          | `Text text -> 
            let entrywithargs = { lastentry with valargs = text >> (fun s -> `Valargs s) } in
            let nentries = alertentry entrywithargs entries in
            parse nentries t ExpectTerminateArgs
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Newline -> parsedefault ExpectProgram
              | _ -> unmatched end 
            | _ -> unmatched end
          | `Empty _ -> parse_rec end
        | ExpectTerminateArgs ->
          let unmatched = synsep Newline |> symval2readable |> unexpectedsymbol_error h in
          begin match h.value with
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Newline -> parsedefault ExpectIndentation
              | Semicolon -> parsedefault ExpectProgram
              | _ -> unmatched end 
            | EOF -> Ok entries end
          | _ -> unmatched end
        | ExpectIndentation -> 
          let unmatched = Tab |> empty2readable |> unexpectedsymbol_error h in
          begin match h.value with
          | `Text text -> 
            let swapentries = swap_lastentry entries lastentry in
            parse (newentry text :: swapentries) t ExpectColonSeparator
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Newline -> parse_rec
              | _ -> unmatched end 
            | EOF -> Ok entries end
          | `Empty _ -> parsedefault ExpectProgram end
        | ExpectProgram -> 
          let unmatched = "Program" |> unexpectedsymbol_error h in
          begin match h.value with
          | `Text text -> 
            let entrywithprogram = { lastentry with program = text >> (fun s -> `Program s) } in
            let nentries = alertentry entrywithprogram entries in
            parse nentries t ExpectCloseEntry
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Newline -> parsedefault ExpectIndentation
              | _ -> unmatched end 
            | EOF -> 
              let swapentries = swap_lastentry entries lastentry in
              Ok swapentries end
          | `Empty _ -> parse_rec end
        | ExpectCloseEntry -> 
          let unmatched = synsep Newline |> symval2readable |> unexpectedsymbol_error h in
          begin match h.value with
          | `Separator sep -> begin match sep with 
            | Syntactsep syntactsep -> begin match syntactsep with 
              | Newline -> parsedefault ExpectEntry
              | _ -> unmatched end 
            | EOF -> Ok entries end
          | _ -> unmatched end end
    | [] -> Ok entries in
  parse [] symbols ExpectEntry
