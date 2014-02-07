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
%token <string> NAME LINE
%token <int> MARK
%token SONG_TITLE


%left PLUS MINUS
%left MULTIPLY DIVIDE
%left NEG	/* negation -- unary minus */
%right CARET	/* exponentiation */

%start input
%type <Data.Song.t> input
%type <Data.Song.t> song 
%type <Data.Song.t> song_data
%type <Chord.t> chord
%type <Chord.t list> chord_list


%%
input : song  { printf "ok, input done, song name is %s\n" $1.Song.name ; flush stdout ; $1 }
| error {
    printf "PARSE ERROR 1 !\nstructure of song is \n\
\\begin(name of the song)\n\
data\n\
\\end\n\
" ; flush stdout ; failwith "bad parse"
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
    { Song.name ="" ; sections = PMap.create String.compare ; structure = [] ; lyrics = [] }
  }
    

song_data  : { { Song.name = "" ; sections = PMap.create String.compare ; structure = [] ; lyrics = [] } }
           | SONG_TITLE LBRACE NAME RBRACE song_data {
	       { $5 with Song.name = $3 ^ $5.Song.name} 
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

structure : BEGIN_STRUCTURE LBRACE section_name_list RBRACE {
  $3
}

name_or_newline : NEWLINE { "\n" }
	| NAME { $1 }

work_with_mark_list : name_or_newline { [ (None,$1) ] }
	   | MARK name_or_newline { [ Some $1,$2 ] }
	   | work_with_mark_list MARK name_or_newline { $1 @ [ Some $2,$3 ] }
	   | work_with_mark_list name_or_newline { $1 @ [ None,$2 ] }


lyrics : BEGIN_LYRICS LBRACE NAME work_with_mark_list RBRACE {
  $3,$4
}

section_name_list : { [] }
	   | NAME { [ $1 ] }
	   | section_name_list NAME { $1 @ [ $2 ] }

chord : CHORD { $1 }

chord_list :  { [] }
	   | chord { [ $1 ] }
	   | chord_list chord { $1 @ [ $2 ] }

section :  BEGIN_SECTION LBRACE NAME chord_list RBRACE {
  { Section.name=$3 ; chords=$4 }
}

;

%%

