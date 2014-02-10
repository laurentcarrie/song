%{

open Printf
open Data


%}


/* Ocamlyacc Declarations */
%token NEWLINE
%token LPAREN RPAREN  LBRACE RBRACE
%token <float> NUM
%token PLUS MINUS MULTIPLY DIVIDE CARET
%token <Data.Chord.t> CHORD
%token BEGIN_SONG BEGIN_SECTION BEGIN_STRUCTURE BEGIN_LYRICS
%token END
%token EOF
%token <string> NAME LINE DIGITS TEXT
%token <int> MARK
%token SONG_FORMAT
%token SECTION_MESURE_PAR_LIGNE
%token TITLE
%token GRILLE
%token BEGIN_TEXT
%token BEGIN_OUTPUT 
%token BEGIN_POSITION_LYRICS
%token BEGIN_POSITION_GRILLE
%token BEGIN_POSITION_STRUCTURE

%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG	/* negation -- unary minus */
%right CARET	/* exponentiation */

%start input
%type <Data.Song.t> input
%type <Data.Song.t> song 
%type <Data.Song.t> song_data
%type <Data.Chord.t> chord
%type <Data.Chord.t list> chord_list
%type <Data.Section.t> section_data section
%type <Data.Lyrics.t> lyrics
%type <Data.Lyrics.t> lyrics_data 
%type <Data.Output.t> output_data
%type <Data.Output.position> position

%%
input : song  { (* printf "ok, input done, song name is %s\n" $1.Song.name ; flush stdout ; *) $1 }
| error {
    printf "error near line %d\n" !line_number ; flush stdout ;
    exit 1 ;
  }

song : BEGIN_SONG LBRACE song_data RBRACE {
  $3
} 
| error {
    printf "PARSE ERROR 2 !\nstructure of song is \n\
\\begin(name of the song)\n\
data\n\
\\end\n\
" ; flush stdout ;
    { Song.name ="" ; format = None ; sections = PMap.create String.compare ; structure = [] ; lyrics = [] ; outputs = [] ; }
  }
    

song_data  : { 	       
              { Song.name = "" ; format = None ; sections = PMap.create String.compare ; structure = [] ; lyrics = [] ; outputs = [] ; } }
           | song_data TITLE LBRACE NAME RBRACE {
	       
               { $1 with Song.name = $4 ^ $1.Song.name} 
	     }
           | song_data SONG_FORMAT LBRACE NAME RBRACE {
	       
	       { $1 with Song.format = Some $4 }
	     }
	   | song_data section {
	       
	       { $1 with Song.sections = PMap.add $2.Section.name $2 $1.Song.sections }
	     }
	   | song_data structure {
	       
	       { $1 with Song.structure = $2 }
	     }
	   | song_data lyrics {
	       
	       { $1 with Song.lyrics = $1.Song.lyrics @ [$2] }
	     }
	   | song_data output {
	       
	       { $1 with Song.outputs = $1.Song.outputs @ [$2] }
	     }
	   | song_data error {
	       printf "parse error in song near line %d\n" !line_number ; flush stdout ;
	       exit(1)
	     }
	       

structure : BEGIN_STRUCTURE LBRACE section_name_list RBRACE {
  $3
}

name_or_newline : NEWLINE { "\n" }
	| NAME { $1 }

text : TEXT { $1 }
        | NAME { $1 }
	| text TEXT { $1 ^ $2 }
	| text NAME { $1 ^ $2 }

work_with_mark_list : name_or_newline { [ (None,$1) ] }
	   | MARK name_or_newline { [ Some $1,$2 ] }
	   | work_with_mark_list MARK name_or_newline { $1 @ [ Some $2,$3 ] }
	   | work_with_mark_list name_or_newline { $1 @ [ None,$2 ] }


position : DIGITS
{
  let position = $1 in
  let position = Str.split (Str.regexp " ") position in
  let position = match position with 
    | [top;left;width;height] -> (
	{ Data.Output.
	    top = int_of_string top ;
	  left = int_of_string left ;
	  width = int_of_string width ;
	  height = int_of_string height ;
		     }
      )
    | _ -> failwith ("position : top left width height") ;
  in
    position
}
output_data : 
           | { { Output.name="" ; lyrics=None ; grille=None ; structure=None ; } }
           | output_data TITLE LBRACE NAME RBRACE { 
	       { $1 with Output.name = $4 } }
           | output_data BEGIN_POSITION_LYRICS LBRACE position RBRACE {
	       { $1 with Output.lyrics = Some $4 }}
           | output_data BEGIN_POSITION_GRILLE LBRACE position RBRACE {
	       { $1 with Output.grille = Some $4 }}
           | output_data BEGIN_POSITION_STRUCTURE LBRACE position RBRACE {
	       { $1 with Output.structure = Some $4 }}
	   | output error {
	       printf "parse error in output definition near line %d\n" !line_number ; flush stdout ;
	       exit(1)
	     }

output : BEGIN_OUTPUT LBRACE output_data RBRACE { $3 }



lyrics_data:
           | { { Lyrics.name="" ; Lyrics.text="" ; Lyrics.index=None } }
           | lyrics_data TITLE LBRACE NAME RBRACE { 
	       { $1 with Lyrics.name = $4 } }
           | lyrics_data BEGIN_TEXT text RBRACE {
               { $1 with Lyrics.text = $3 }}
	   | lyrics_data error {
	       printf "parse error in lyrics near line %d\n" !line_number ; flush stdout ;
	       exit(1)
	     }


lyrics : 
           | BEGIN_LYRICS LBRACE lyrics_data RBRACE  { $3 }

section_name_list : { [] }
	   | section_name_list NAME { $1 @ [ $2 ] }
	   | NAME { [ $1 ] }

chord : CHORD {
  $1
}


chord_list :  
	   | chord_list chord { 
	       $1 @ [ $2 ] 
	     }
	   | chord {  [ $1 ] }
section_data : 
           | {{ Section.name="" ; mesures_par_ligne=None ; chords=[] } }
	   | section_data TITLE LBRACE NAME RBRACE {
	       { $1 with Section.name=$4 }
	     }
	   | section_data SECTION_MESURE_PAR_LIGNE LBRACE DIGITS RBRACE  {
	       { $1 with Section.mesures_par_ligne=__SONG__try "split string to int list" 
		   (Some ( List.map int_of_string (Str.split (Str.regexp " ") $4))) }
	     }
           | section_data GRILLE LBRACE chord_list RBRACE   {
               { $1 with Section.chords=$4 }
             }
           | section_data error  {
	       printf "parse error in section, near line %d\n" !line_number ; flush stdout ;
	       exit(1)
             }

section :  BEGIN_SECTION LBRACE section_data RBRACE {
	       
  $3
}

;

%%

