open Sys
open Types
open Symbols
open Entries

let vest_file_cap = "Vestfile"
let vest_file_low = "vestfile"

let openvestfile vestfile = 
  try 
    let vestfilc = open_in vestfile in
    Ok vestfilc
  with _ -> vestfilerr "error: Can not open Vestfile"

let find_vestfile () = 
    if file_exists vest_file_cap
    then Ok vest_file_cap
    else if file_exists vest_file_low
    then Ok vest_file_low
    else vestfilerr "error: No Vestfile found in the current directory"

let rec print f = function 
  | [] -> ()
  | h::t -> f h |> print_string ;
  print_char ',' ;
  print_endline "";
  print f t 

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
