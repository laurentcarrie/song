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
	       tempo=80 ;} in
    pf "lecture du repertoire %s<br/>" dirname ; 
    log "lecture du repertoire : '%s'" dirname ;
    let (song,do_it) = __SONG__try ("song " ^ dirname) (
      let song = Grille_of_file.read_file song    (dirname // "grille.txt") in
      let song = Lyrics_of_file.read_file song    (dirname // "lyrics.txt") in
      let song = Structure_of_file.read_file song (dirname // "structure.txt") in
      let song = Main_of_file.read_file song      (dirname // "main.txt") in
      let song = Sortie_of_file.read_file song (dirname // "sortie.txt") in
      let song = Check.check song in
      let filename_digest = output_dir // (song.Song.filename ^ ".digest" ) in
      let () = log "filename_digest = %s" filename_digest in
      let last_digest_hex = 
	if Sys.file_exists filename_digest then Some(Std.input_file filename_digest) else None
      in
      let watched_files = 
	( List.map ( fun f -> dirname // f ) [
	    "grille.txt" ;
	    "lyrics.txt" ;
	    "structure.txt" ;
	    "main.txt" ;
	    "sortie.txt" ;
	  ] ) @
	  ( List.map ( fun o -> output_dir // (o.Output.filename  ^ ".html")) song.Song.outputs ) 
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
	      pf "at %02d/%02d/%04d %02d:%02d:%02d" 
		tm.Unix.tm_mday (tm.Unix.tm_mon+1) (tm.Unix.tm_year+1900)
		tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
	  ) ;
	  Html.render_html song output_dir ;
	  Lilypond.render song output_dir ;
	  let digest = compute_digest false in
	  let () =  match digest with
	    | None -> __SONG__failwith "internal error"
	    | Some digest -> Std.output_file ~filename:filename_digest ~text:(Digest.to_hex digest) ;
	  in
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
  
