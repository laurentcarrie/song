open Printf
open Data
open Util

let (//) = Filename.concat 

type args = ( string * string ) list



let find_all_songs ~plog root = __SONG__try "find all songs" (
  let log fs = ksprintf plog fs in
  let rec find acc root =
    let dirs = Sys.readdir root in
    let dirs = Array.to_list dirs in
      List.fold_left ( fun acc d ->
	let d = root//d in
	let () = log "Reading directory %s" d in
	let acc = (
	  if  Sys.is_directory d then (
	    if Filename.check_suffix d ".song" then  d :: acc  else find acc d
	  ) else (
	    acc
	  )
	) in
	  acc
      ) acc dirs 
  in
    find [] root
)


let generate   ~root ~output_dir ~doc_root ~relative_output_dir   ~root_path  ~plog ~print dirname  = __SONG__try ("generate song : " ^ dirname) (
  let pf fs =  ksprintf print fs in
  let log fs = ksprintf plog fs in
  let song = { Song.title="???" ; Song.auteur="???" ; filename = "no-filename"  ^ (string_of_int (Random.int 1000)) ; format=None ; sections=PMap.create String.compare ; structure=[];lyrics=[];outputs=[];
	       digest=Digest.string ""; tempo=80 ;} in
    pf "lecture du repertoire %s<br/>" dirname ; 
    log "lecture du repertoire : '%s'" dirname ;
    let (song,do_it) = __SONG__try ("song " ^ dirname) (
      let song = Grille_of_file.read_file song    (dirname // "grille.txt") in
      let song = Lyrics_of_file.read_file song    (dirname // "lyrics.txt") in
      let song = Structure_of_file.read_file song (dirname // "structure.txt") in
      let song = Main_of_file.read_file song      (dirname // "main.txt") in
      let song = Sortie_of_file.read_file song (dirname // "sortie.txt") in
      let song = Check.check song in
      let old_digest_hex = 
	match Sys.file_exists (dirname//"digest.txt") with
	  | true -> (try
		Some (Std.input_file  (dirname//"digest.txt"))
	    with
	      | e -> log "ERROR : %s" (Printexc.to_string e) ; None
	    )
	  | false -> None
      in
      let do_it = match old_digest_hex with
	| None -> log "DIGEST not found"; true
	| Some d -> 
	    let do_it = (Digest.to_hex song.Song.digest) <> d in 
	      log "DIGEST do_it=%b ; %s ; %s" do_it (Digest.to_hex song.Song.digest) d ;
	      do_it
      in
	if do_it then (
	  Std.output_file ~filename:(dirname//"digest.txt") ~text:(Digest.to_hex song.Song.digest) ;
	  (
	    let tm = Unix.localtime(Unix.time ()) in
	      pf "at %02d/%02d/%04d %02d:%02d:%02d" 
		tm.Unix.tm_mday (tm.Unix.tm_mon+1) (tm.Unix.tm_year+1900)
		tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
	  ) ;
	  Html.render_html song output_dir ;
	  Lilypond.render song output_dir ;
	  List.iter ( fun output ->
	    pf "Ã©criture de <a href=\"%s%s/%s.html\">%s.html</a><br/>\n" root_path relative_output_dir output.Output.filename output.Output.filename
	  ) song.Song.outputs ;
	  song,do_it
	) else (
	  List.iter ( fun output ->
	    pf "Déjà à jour : <a href=\"%s%s/%s.html\">%s.html</a><br/>\n" root_path relative_output_dir output.Output.filename output.Output.filename
	  ) song.Song.outputs ;
	  song,do_it
	)
	  
    ) in
      song,do_it
)
  
