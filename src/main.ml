open Printf
open Data


let main () = __SONG__try "main" (
  let in_filename = Sys.argv.(1) in
  let () = if Filename.check_suffix in_filename "song" then () else failwith "input file should have '.song' extension" in
  let out_filename = (Filename.chop_extension in_filename) ^ ".html" in
  let fin = open_in_bin in_filename in
    try
      let lexbuf = __SONG__try "lexing" (Lexing.from_channel fin) in
      let song = __SONG__try "bisoning" (Song_bison.input Song_lex.token lexbuf) in
	(*
	  printf "OK : name is %s\n\n" song.Song.name ; 
	  PMap.iter ( fun _ s ->
	  printf "section %S\n" s.Section.name ;
	  List.iter ( fun c -> printf "%c " c.Chord.name ) s.Section.chords ;
	  printf "\n"
	  ) song.Song.sections ;
	  flush stdout ;
	*)
	Html.render_html song out_filename  ;
	printf "%s -> %s\n" in_filename out_filename 
    with End_of_file -> printf "EOF\n" ; flush stdout ; close_in fin ; exit 0
)
      
let _ = try 
    main () ;
  exit 0
  with  
    | e -> let () = __SONG__print_exn_stack e in exit(1)
    

