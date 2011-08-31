(* file: lexer.mll *)
(* Lexical analyzer returns one of the tokens:
   the token NUM of a floating point number,
   operators (PLUS, MINUS, MULTIPLY, DIVIDE, CARET, UMINUS),
   or NEWLINE.  It skips all blanks and tabs, and unknown characters
   and raises End_of_file on EOF. *)

{
  open Song_bison
  open Data
  open Printf
  let dp a b = printf "%s -> %s\n" a b ; flush stdout 

}
let digit = ['0'-'9']
let chord = ['a'-'g']
let name =  ['a'-'z' 'A'-'Z' '0'-'9'] ['a'-'z' 'A'-'Z' '0'-'9' ' ']*
let id =  ['a'-'z' 'A'-'Z' '0'-'9']+


rule token = parse
  | [' ' '\t']+	{ token lexbuf  }
  | '\n'	{ token lexbuf (* NEWLINE *) }
  | digit+
  | "." digit+
  | digit+ "." digit* as num
		{ NUM (let f = float_of_string num in f ) }
  | '+'		{ PLUS }
  | '-'		{ MINUS }
  | '*'		{ MULTIPLY }
  | '/'		{ DIVIDE }
  | '^'		{ CARET }
  | '('		{ LPAREN }
  | ')'		{ RPAREN }
  | '{'		{ LBRACE }
  | '}'		{ RBRACE }
  | (['1'-'3']? as length) "\\" (chord as c) ('m'? as minor) { 
      printf "lex chord %c\n" c ; flush stdout ; 
      let length = match length with 
	| "" -> 4
	| s  -> int_of_string s
      in
      let minor = (minor = "m") in
	CHORD { cname=c ; clength=length; minor=minor ; mi7=false ; ma7=false}
    }  
  | "\\song"     { BEGIN_SONG } 
  | "\\song_title"     { dp "song_title" "" ; SONG_TITLE } 
  | "\\end"     { END } 
  | "\\section"     { BEGIN_SECTION } 
  | "\\structure"     { dp "structure" ""  ;  BEGIN_STRUCTURE } 
  | '"' (name as c) '"'   {   printf "name : %S\n" c ; flush stdout ; NAME c }
  | id as c   {   printf "id : %S\n" c ; flush stdout ; NAME c }
  | _		{ token lexbuf }
  | eof		{ raise End_of_file }
