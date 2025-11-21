{

  open Lexing
  open Mgoparser

  exception Error of string

  let keyword_or_ident =
  let h = Hashtbl.create 17 in
  List.iter (fun (s, k) -> Hashtbl.add h s k)
    [ "else",    ELSE ;
      "false",   FALSE;
      "for",     FOR;
      "func",    FUNC;
      "if",      IF;
      "import",  IMPORT;
      "nil",     NIL;
      "package", PACKAGE;
      "return",  RETURN;
      "struct",  STRUCT;
      "true",    TRUE;
      "type",    TYPE;
      "var",     VAR;
    ] ;
  fun s ->
    try  Hashtbl.find h s
    with Not_found -> IDENT(s)
        
}

let digit = ['0'-'9']
let hexa = ['0'-'9' 'a'-'f' 'A'-'F']
let number = (digit+ | ("0x" | "0X") hexa+)
let alpha = ['a'-'z' 'A'-'Z' '_']
let ident = alpha (alpha | digit)*
let ascii = ['\032'-'\126']
let car = ( ascii - ['"' '\'] | '\\' | '\"' | '\n' | '\t')
let chaine = '"' car* '"'

rule token = parse
  | ['\n']            { new_line lexbuf; token lexbuf }
  | [' ' '\t' '\r']+  { token lexbuf }

  | "/*"              { comment lexbuf; token lexbuf }
  | "//" [^'\n']*     { token lexbuf }

  | '"'               { string lexbuf ""}

  | number as n  { try INT(Int64.of_string n) 
                   with _ -> raise (Error "literal constant too large") }
  | ident as id  { keyword_or_ident id }

  | ";"  { SEMI }
  | "("  { LPAR }
  | ")"  { RPAR }
  | "{"  { BEGIN }
  | "}"  { END }
  | "*"  { STAR }

  | _    { raise (Error ("unknown character : " ^ lexeme lexbuf)) }
  | eof  { EOF }

and comment = parse
  | '\n' { new_line lexbuf; comment lexbuf }
  | "*/" { token lexbuf }
  | _    { comment lexbuf }
  | eof  { raise (Error "unterminated comment") }
  
and string = parse
  | '"'                { STRING("") }
  | "\\n"              { let s = "\n" in s ^ (string lexbuf) }
  | "\\t"              { let s = "\t" in s ^ (string lexbuf) }
  | "\\\""             { let s = "\"" in s ^ (string lexbuf) }
  | "\\\\"             { let s = "\\" in s ^ (string lexbuf) }
  | [^ '"' '\\']+ as s {s ^ (string lexbuf)}
  | eof                { raise (Error "unterminated string") }