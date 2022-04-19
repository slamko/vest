type valargs = [ `Valargs of string ]
type program = [ `Program of string ]
type entryname = [ `Entryname of string ]
type symtext = Symtext of string
type operror =
    Nothing of string
  | VestfileError of string
  | SyntaxError of string
type token =
    Space
  | Newline
  | Colon
  | Tab
  | Char of char
  | EOF
type syntactsep = Colon | Newline
type separator  = Syntactsep of syntactsep | EOF
type empty = Space | Tab
type basesym = [ `Separator of separator | `Text of symtext ]
type symval =
    [ `Empty of empty | `Separator of separator | `Text of symtext ]
type sympos = { linenum : int; }
type symbol = { value : symval; pos : sympos; }
type parsesym = {
  symbol : symtext;
  separator : separator;
}
type valentry = {
  name : entryname;
  valargs : valargs;
  program : program;
}
type parsestate =
    ExpectEntry
  | ExpectColonSeparator
  | ExpectValargs
  | ExpectTerminateArgs
  | ExpectProgram
  | ExpectCloseEntry
val ( ++ ) : symtext -> symtext -> symtext
val sunwrap : symtext -> string
val ( >> ) : symtext -> (string -> 'a) -> 'a
val ( << ) : string -> symtext
val ( >>= ) : ('a, 'b) result -> ('a -> ('c, 'b) result) -> ('c, 'b) result
val tosym : string -> symtext
val tofsymt : 'a -> [> `Text of 'a ]
val sep2str : separator -> string
val empty2str : empty -> string
val symval2str :
  [< `Empty of empty | `Separator of separator | `Text of symtext ] ->
  string
val sym2str : symbol -> string
val entryval2str :
  [< `Entryname of 'a | `Program of 'a | `Valargs of 'a ] -> 'a
val entry2str : valentry -> string
val synsep : syntactsep -> [> `Separator of separator ]
val unexpectedsymbol_error : symbol -> string -> ('a, operror) result
val prerr : operror -> unit
val synterror : string -> ('a, operror) result
val syserror : string -> ('a, operror) result
val nothing : string -> ('a, operror) result
val vestfilerr : string -> ('a, operror) result