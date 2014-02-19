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

let debug = true

let dp0 a = if debug then (printf  "%d ; %s\n" !line_number a  ; flush stdout ) else ()
let dp a b = if debug then (printf "%d ; %s -> %S\n" !line_number a b ; flush stdout ) else ()

let newline () = incr line_number
}

let digit = ['0'-'9']
let chord = ['a'-'g']
let alteration = ['b' '#']
let name_with_space =  ['a'-'z' 'A'-'Z' '0'-'9'] ['a'-'z' 'A'-'Z' '0'-'9' ',' ' ' ]+
let name =  ['a'-'z' 'A'-'Z' '0'-'9'] ['a'-'z' 'A'-'Z' '0'-'9' ',' ]+
let comment = "//"  ['a'-'z' 'A'-'Z' '0'-'9' ' ' ',']* '\n'
let digits = ['0'-'9'] ['0'-'9' ' ']+
(* let text = ['/' 'a'-'z' 'A'-'Z' '0'-'9' ',' ' ' '\n' '.' '\'' '\"' '<' '>' '/' '#']+ *)
  let text = [^'}']+

    rule token = 
    parse
	| comment as c  { dp "comment : " c ; token lexbuf }
	| [' ' '\t']+ as c {  match !context with | OUT_TEXT -> dp0 "skipped space" ; token lexbuf  | IN_TEXT -> dp0 "in text space" ; TEXT c }
	| '\n'  {  newline() ; match !context with | OUT_TEXT -> dp0 "skipped newline" ; token lexbuf  | IN_TEXT -> dp0 "in text newline" ; TEXT "\n" }
	| digit+ "." digit* as num
	    { NUM (let f = float_of_string num in f ) }
	| '{'		{ match !context with OUT_TEXT -> dp0 "LBRACE" ; LBRACE | IN_TEXT -> dp0 "LBRACE ignore" ; token lexbuf }
	| '}'		{ dp0 "RBRACE" ; context := OUT_TEXT ; RBRACE }
	| (['1'-'3']? as length) "\\" (chord as c) (alteration? as alteration) ('m'? as minor) { 
	    dp "lex chord " (sprintf "c=%c a=%s m=%s" c alteration minor) ;
	    let note = { Data.Note.letter = c ; is_7 = false ; is_7M = false ; is_m = (minor = "m") ; is_flat = (alteration="b") ; is_sharp=(alteration="#") } in
	    let length = match length with 
	      | "" -> 4
	      | s  -> int_of_string s
	    in
	      CHORD { Data.Chord.note=note ; length=length; }
	  }  
	| "\\song"     { dp "BEGIN_SONG" "" ;  BEGIN_SONG } 
	| "\\title"     { dp0 "title"  ; TITLE } 
	| "\\mesures_par_ligne"     { dp "mesures par ligne" "" ; SECTION_MESURE_PAR_LIGNE } 
	| "\\end"     { END } 
	| "\\section"     { dp0 "section"  ; BEGIN_SECTION } 
	| "\\lyrics"     { BEGIN_LYRICS } 
	| "\\structure"     { dp0 "structure"   ;  BEGIN_STRUCTURE } 
	| "\\format" { SONG_FORMAT  }
	| "\\text" { context := IN_TEXT ; dp0 "BEGIN_TEXT" ; BEGIN_TEXT }
	| "\\grille" { dp0 "GRILLE" ; GRILLE }
	| "\\output" { dp0 "OUTPUT" ; BEGIN_OUTPUT }
	| "\\position_lyrics"  { dp0 "POSITION_LYRICS" ; BEGIN_POSITION_LYRICS  }
	| "\\position_grille"  { dp0 "POSITION_GRILLE" ; BEGIN_POSITION_GRILLE  }
	| "\\position_structure"  { dp0 "POSITION_STRUCTURE" ; BEGIN_POSITION_STRUCTURE  }
        | "\\" name as key { failwith ("no such keyword : " ^ key) }
	| '"' ((name_with_space | ' ')+ as c) '"'   {   dp "name0" c ; NAME c }
	| (name as c)    {   dp "name" c ; NAME c }
	| _  as c {  match !context with | OUT_TEXT -> dp0 "skipped text" ; token lexbuf  | IN_TEXT -> dp "in text" (sprintf "%c" c) ; TEXT (sprintf "%c" c) }
	| (digits as c)  { dp "digits" c ; DIGITS c }
	| eof		{ raise End_of_file }

