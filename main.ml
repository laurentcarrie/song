open Printf
open Data


let main () =
  let fin = open_in_bin Sys.argv.(1) in
    try
      let lexbuf = Lexing.from_channel fin in
      let song = Song_bison.input Song_lex.token lexbuf in
	printf "OK : name is %s\n\n" song.Song.name ; 
	PMap.iter ( fun _ s ->
	  printf "section %S\n" s.Section.name ;
	  List.iter ( fun c -> printf "%c " c.Chord.name ) s.Section.chords ;
	  printf "\n"
	) song.Song.sections ;
	flush stdout ;
	Html.render_html song "out.html" 
    with End_of_file -> printf "EOF\n" ; flush stdout ; close_in fin ; exit 0
      
let _ = Printexc.print main ()
    

