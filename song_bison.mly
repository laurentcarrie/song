%{

open Printf
open Data

%}


/* Ocamlyacc Declarations */
%token NEWLINE
%token LPAREN RPAREN  LBRACE RBRACE
%token <float> NUM
%token PLUS MINUS MULTIPLY DIVIDE CARET
%token <Data.chord_t> CHORD
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
%type <Data.song_t> input
%type <song_t> song 
%type <song_t> song_data
%type <chord_t> chord
%type <chord_t list> chord_list


%%
input : song  { printf "ok, input done, song name is %s\n" $1.name ; flush stdout ; $1 }
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
    { name ="" ; sections = PMap.create String.compare ; structure = [] ; lyrics = [] }
  }
    

song_data  : { { name = "" ; sections = PMap.create String.compare ; structure = [] ; lyrics = [] } }
           | SONG_TITLE LBRACE NAME RBRACE song_data {
	       { $5 with name = $3 ^ $5.name} 
	     }
	   | section song_data {
	       { $2 with sections = PMap.add $1.sname $1 $2.sections }
	     }
	   | structure song_data {
	       { $2 with structure = $1 }
	     }
	   | lyrics song_data {
	       { $2 with lyrics = $2.lyrics @ [$1] }
	     }

structure : BEGIN_STRUCTURE LBRACE section_name_list RBRACE {
  $3
}

work_with_mark_list : NAME { [ (None,$1) ] }
	   | MARK NAME { [ Some $1,$2 ] }
	   | work_with_mark_list MARK NAME { $1 @ [ Some $2,$3 ] }
	   | work_with_mark_list NAME { $1 @ [ None,$2 ] }


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
  { sname=$3 ; chords=$4 }
}

;

%%

