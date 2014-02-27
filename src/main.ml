open Printf
open Data
open ExtString

let (//) = Filename.concat
  
(* let here = Filename.dirname Sys.argv.(0) *)
let logfile = "log.html"
let flog = open_out_bin logfile

let log s =
  fprintf flog "%s\n" s ;
  flush flog

let manage_song dirname  = 
  try
    printf "reading song in : '%s'\n" dirname ;
    let song = { Song.title="???" ; Song.auteur="???" ; filename = "no-filename-" ^ (string_of_int (Random.int 1000))  ; format=None ; sections=PMap.create String.compare ; structure=[];lyrics=[];outputs=[];
		 tempo = 80 } in
    let song = Grille_of_file.read_file song (dirname // "grille.txt") in
    let song = Lyrics_of_file.read_file song (dirname // "lyrics.txt") in
    let song = Structure_of_file.read_file song (dirname // "structure.txt") in
    let song = Main_of_file.read_file song (dirname // "main.txt") in
    let song = Sortie_of_file.read_file song (dirname // "sortie.txt") in
      Html.render_html song "songs"
      
  with
    | e -> log (Song_exn.html_string_of_stack () ) ;  let () = __SONG__print_exn_stack e in Song_exn.clear_stack() ; ()


(*
let main() = __SONG__try "main" (

  let fcatalog = __SONG__try "open catalog" (open_in_bin Sys.argv.(1)) in
  let rec read () = 
    try
      let line = input_line fcatalog in
      let line = String.strip line in
	manage_song line ;
	read ()
    with
      | End_of_file -> close_in fcatalog ; ()
  in
    
    read ()
)
*)




let main() = __SONG__try "main" (
  
  let root = Sys.argv.(1) in
  let dirs = Sys.readdir root in
  let dirs = Array.to_list dirs in
  let dirs = List.filter ( fun d -> Sys.is_directory (root // d)) dirs in
  let dirs = List.filter ( fun d -> Filename.check_suffix (root//d) ".song" ) dirs in
    
    List.iter ( fun d ->
		  let () = log (sprintf "reading %s<br>\n" (root//d)) in
		    manage_song (root // d)
	      ) dirs
)
  



					   

let _ = try
  let () = if Sys.file_exists logfile then Unix.unlink logfile else () in
  let () = main () in
  let () = Std.output_file ~filename:logfile ~text:"no problem found, files generated" in
    exit 0
with  
  | e -> 
      let msg = Song_exn.html_string_of_stack () in
      let () = Std.output_file ~filename:logfile ~text:msg in
      let () = __SONG__print_exn_stack e in ()
