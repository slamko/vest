open Types

val eval_entries: valentry list -> (unit, operror) result 

val check_entries: valentry list -> (valentry list, operror) result 

val parse_entries: symbol list -> (valentry list, operror) result 