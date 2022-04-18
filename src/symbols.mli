open Types

val clean_emptysymbs: symbol list -> (symbol list, 'a) result

val parse_symbols: in_channel -> (symbol list, operror) result