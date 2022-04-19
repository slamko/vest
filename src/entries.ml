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
  | [] -> nothing "No entries found in Vestfile\n"
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
  
let parse_entries symbols : (valentry list, operror) result  = 
  let rec parse expected entries symbols = match symbols with 
    | h::t ->
        let lastentry = entries |> preventry in
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
            | EOF -> parse ExpectEntry entries t end
          | _ -> unmatched end end
    | [] -> Ok entries in
  parse ExpectEntry [] symbols
