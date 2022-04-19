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
  | Semicolon
  | Tab
  | Char of char
  | EOF

type syntactsep = 
  | Colon
  | Semicolon
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
  | ExpectIndentation
  | ExpectProgram
  | ExpectCloseEntry

let (++) symb symapp : symtext = 
  match symb with 
  | Symtext s -> begin match symapp with 
    | Symtext sapp -> Symtext (s ^ sapp) end

let sunwrap = function 
  | Symtext s -> s

let (>>) s f = sunwrap s |> f 

let (<<) s = Symtext s

let (>>=) r f = match r with 
  | Ok value -> f value
  | Error err -> Error err

let tosym = (<<)

let tofsymt symtext = 
  `Text symtext

let sep2str = function 
  | Syntactsep sep -> 
    begin match sep with 
    | Newline -> "New line"
    | Colon -> "Colon" 
    | Semicolon -> "Semicolon" end
  | EOF -> "EOF"

let empty2str = function 
  | Space -> " "
  | Tab -> "\t" 

let empty2readable = function 
  | Space -> "Space"
  | Tab -> "Tab" 

let symval2str = function 
  | `Text text -> sunwrap text 
  | `Separator sep -> sep2str sep
  | `Empty empty -> empty2str empty

let symval2readable = function 
  | `Text text -> sunwrap text 
  | `Separator sep -> sep2str sep
  | `Empty empty -> empty2readable empty


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

let synterror err = Error (SyntaxError  ("error: " ^ err ^ "\n"))
let syserror err = Error (SyntaxError  ("error: " ^ err ^ "\n"))
let nothing err = Error (Nothing  ("error: " ^ err ^ "\n"))
let vestfilerr err = Error (VestfileError ("error: " ^ err ^ "\n"))
