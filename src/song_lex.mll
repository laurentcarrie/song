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

  let debug = false

  let dp a b = if debug then (printf "%s -> %s\n" a b ; flush stdout ) else ()
}

let digit = ['0'-'9']
let chord = ['a'-'g']
let alteration = ['b' '#']
let name =  ['a'-'z' 'A'-'Z' '0'-'9'] ['a'-'z' 'A'-'Z' '0'-'9' ',' ]*
let comment = "//"  ['a'-'z' 'A'-'Z' '0'-'9' ' ' ',']* '\n'
let digits = ['0'-'9'] ['0'-'9' ' ']+

rule token = parse
  | comment as c { dp "comment : " c ; token lexbuf }
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
  | (['1'-'3']? as length) "\\empty" { 
      CHORD { Data.Chord.name='-' ; length=int_of_string length;alteration=Data.Rien;minor=false;mi7=false;ma7=false}
    }
  | (['1'-'3']? as length) "\\" (chord as c) (alteration? as alteration) ('m'? as minor) { 
      dp "lex chord " (sprintf "c=%c a=%s m=%s" c alteration minor) ;
      let length = match length with 
	| "" -> 4
	| s  -> int_of_string s
      in
      let minor = (minor = "m") in
      let alteration = match alteration with | "b" -> Data.Flat | "#" -> Data.Sharp | _ -> Data.Rien in
	CHORD { Data.Chord.name=c ; length=length; alteration=alteration;minor=minor ; mi7=false ; ma7=false}
    }  
  | "\\song"     { dp "BEGIN_SONG" "" ;  BEGIN_SONG } 
  | "\\song_title"     { dp "song_title" "" ; SONG_TITLE } 
  | "\\section_title"     { dp "section_title" "" ; SECTION_TITLE } 
  | "\\mesures_par_ligne"     { dp "mesures par ligne" "" ; SECTION_MESURE_PAR_LIGNE } 
  | "\\end"     { END } 
  | "\\section"     { dp "section" "" ; BEGIN_SECTION } 
  | "\\lyrics"     { BEGIN_LYRICS } 
  | "\\structure"     { dp "structure" ""  ;  BEGIN_STRUCTURE } 
  | "\\format" { SONG_FORMAT  }
  | "\\" (digit+ as num) { MARK (int_of_string num) }
  | "\\n"   { NEWLINE } 
  | '"' ((name | ' ')+ as c) '"'   {   dp "name0" c ; NAME c }
  | (name as c)    {   dp "name" c ; NAME c }
  | (digits as c)  { dp "digits" c ; DIGITS c }
  | _		{ token lexbuf }
  | eof		{ raise End_of_file }
      
