open Printf
open Data
open Util

let (//) = Filename.concat 

type args = ( string * string ) list



let find_all_songs ~plog ~root = __SONG__try "find all songs" (
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


let song_of_path dirname = __SONG__try ("song_of_path " ^ dirname) (
  let song = { Song.title="???" ; Song.auteur="???" ; 
	       filename = "no-filename"  ^ (string_of_int (Random.int 1000)) ; format=None ; sections=[] ; 
	       structure=[];lyrics=[];outputs=[];
	       tempo=80 ;
	       path = dirname ;
	       server_path = "ZZZZ";
  } in
  let song = Grille_of_file.read_file song    (dirname // "grille.txt") in
  let song = Lyrics_of_file.read_file song    (dirname // "lyrics.txt") in
  let song = Structure_of_file.read_file song (dirname // "structure.txt") in
  let song = Main_of_file.read_file song      (dirname // "main.txt") in
  let song = Sortie_of_file.read_file song (dirname // "sortie.txt") in
  let song = Check.check song in
    song
)


let write_song song dirname = __SONG__try ("write song " ^ dirname) (
  let () =
    let fout = open_out_bin (dirname // "grille.txt") in
    let pf fs = kfprintf ( fun fout -> fprintf fout "\n" ) fout fs in
    let pfnnl fs = kfprintf ( fun fout -> () ) fout fs in
      List.iter ( fun section -> 
	pf "%s" section.Section.name ;
	List.iter ( fun c ->
	  match c with
	    | Section.NL -> pf "" ;
	    | Section.C c -> pfnnl  "%s " (Note.text_name c.Chord.note c.Chord.length) ;
	) section.Section.chords ;
	pf "" ;
      ) song.Song.sections ;
      close_out fout
  in
  let () = 
    let fout = open_out_bin    (dirname // "lyrics.txt") in
    let pf fs = kfprintf ( fun fout -> fprintf fout "\n" ) fout fs in
      List.iter ( fun l ->
	pf "%s" l.Lyrics.name ;
	pf "%s" l.Lyrics.text ;
      ) song.Song.lyrics ;
      close_out fout
  in
  let () = 
    let fout = open_out_bin    (dirname // "main.txt") in
    let pf fs = kfprintf ( fun fout -> fprintf fout "\n" ) fout fs in
      pf "\\titre %s\n" song.Song.title ;
      pf "\\auteur %s\n" song.Song.auteur ;
      pf "\\tempo %d\n" song.Song.tempo ;
      pf "\\filename %s\n"  song.Song.filename ;
      close_out fout
  in
    ()
)

let generate_from_song   ~world  ~plog ~print ~path song  = __SONG__try ("generate song : " ^ path) (
  let filename_digest = world.World.output_root // (song.Song.filename ^ ".digest" ) in
  let log fs = ksprintf plog fs in
  let () = log "filename_digest = %s" filename_digest in
  let last_digest_hex = 
    if Sys.file_exists filename_digest then Some(Std.input_file filename_digest) else None
  in
  let watched_files = 
    ( List.map ( fun f -> path // f ) [
      "grille.txt" ;
      "lyrics.txt" ;
      "structure.txt" ;
      "main.txt" ;
      "sortie.txt" ;
    ] ) @
      ( List.map ( fun o -> world.World.output_root // (o.Output.filename  ^ ".html")) song.Song.outputs ) 
  in
  let compute_digest accept_fail  = __SONG__try "compute digest" (
    List.fold_left ( fun acc filename ->
      let exists = match accept_fail,Sys.file_exists filename with
	| _ , true -> true
	| true,false -> false
	| false,false -> __SONG__failwith ("file " ^ filename ^ " n'existe pas")
      in
	match acc, exists with 
	  | None,true
	  | None,false
	  | Some _,false
	      -> None
	  | Some d, true ->
	      Some (Util.update_digest d filename)
    ) ( Some (Digest.string "")) watched_files )
  in
  let digest = compute_digest true  in
  let do_it = match last_digest_hex,digest with
    | None,None
    | None,Some _
    | Some _,None -> log "DIGEST not found"; true
    | Some h,Some d-> 
	let do_it = (Digest.to_hex d) <> h in 
	  log "DIGEST do_it=%b ; %s ; %s" do_it (Digest.to_hex d) h ;
	  do_it
  in
    
    if do_it then (
      (
	let tm = Unix.localtime(Unix.time ()) in
	  log "at %02d/%02d/%04d %02d:%02d:%02d" 
	    tm.Unix.tm_mday (tm.Unix.tm_mon+1) (tm.Unix.tm_year+1900)
	    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      ) ;
      (* Html.render_html world song ;*)
      (* Lilypond.render world song  ; *)
      let digest = compute_digest false in
      let () =  match digest with
	| None -> __SONG__failwith "internal error"
	| Some digest -> Std.output_file ~filename:filename_digest ~text:(Digest.to_hex digest) ;
      in
	List.iter ( fun output ->
	  log "écriture de <a href=\"%s.html\">%s.html</a><br/>\n" output.Output.filename output.Output.filename
	) song.Song.outputs ;
	do_it
    ) else (
      List.iter ( fun output ->
	log "D�j� � jour : <a href=\"%s.html\">%s.html</a><br/>\n" output.Output.filename output.Output.filename
      ) song.Song.outputs ;
      do_it
    )
      
) 

let generate   ~world  ~plog ~print ~path  : (Song.t * bool)= __SONG__try ("generate song : " ^ path) (
  let pf fs =  ksprintf print fs in
  let log fs = ksprintf plog fs in
    pf "lecture du repertoire %s<br/>" path ; 
    let path = normalize_path path in
    log "lecture du repertoire : '%s'" path ;
    log "path = %s" path ;
    let song = song_of_path path in
    let do_it =  __SONG__try ("song " ^ path) (  
      generate_from_song ~world ~plog ~print ~path song )
    in
      song,do_it
)

