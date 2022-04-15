open Sys;;

let vest_file = "Vestfile"

let error_no_vestfile () =
  prerr_endline "error: No Vestfile found in the current directory"

let () = 
  if not @@ file_exists vest_file 
  then error_no_vestfile () ;;

  print_string argv.(0) ;;
  print_endline ""
