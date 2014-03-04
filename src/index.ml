open Util
open Data
open Printf
open ExtList

let (//) = Filename.concat

let log = Fcgi.log

let alphabet = 
  let c1 = Char.code 'A' in
  let c2 = Char.code 'Z' in
  let rec build acc current = 
    if current>c2 then List.rev acc 
    else build ((Char.chr current)::acc) (current+1)
  in
    build [] c1
      
let compare song1 song2 =
  match String.compare song1.Song.auteur song2.Song.auteur with
    | 0 -> String.compare song1.Song.title song2.Song.title
    | i -> i

let write_index print world = __SONG__try "index" (
  log "write index"; 
  let songs = world.World.songs in
  let pf fs = ksprintf ( fun s -> print s ; print "\n" ) fs in
  let songs_assoc = List.fold_left ( fun acc s ->
    let auteur = s.Song.auteur in
      try
	let l = List.assoc auteur acc in
	  (auteur,(s::l)) :: (List.remove_assoc auteur acc)
      with
	| Not_found -> (s.Song.auteur,[s]) :: acc
  ) [] songs
  in
  let songs_alphabet = List.map ( fun letter ->
    (* log "index, examen lettre %c" letter ; *)
    let acc = List.fold_left ( fun acc (auteur,songs) ->
      (* log "... auteur = '%s'" auteur ; *)
      let () = if auteur = "" then __SONG__failwith ("auteur non defini") else () in
      let l2 = Char.uppercase (String.get auteur 0) in
	if letter = l2 then (
	  (* log "... match\n" ;  *)
	  (auteur,songs)::acc
	)
	else (
	  (* log "don't match '%c' '%c'" letter l2 ;  *)
	  acc
	)
    ) [] songs_assoc
    in
      letter,List.rev acc
  ) alphabet in
    
    log "songs alphabet : %d" (List.length songs_alphabet) ;
    (
      List.iter ( fun (s,_) -> log "auteur : %s" s ) songs_assoc
    ) ;
    
    
    (
      let tm = Unix.localtime(Unix.time ()) in
	pf "index généré le  %02d/%02d/%04d à  %02d:%02d:%02d</br>" 
	  tm.Unix.tm_mday (tm.Unix.tm_mon+1) (tm.Unix.tm_year+1900)
	  tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
    ) ;
    pf "<a href='/download.songx'>Télécharger sous forme d'un fichier zip</a>" ;
    pf "<div id=\"list5\" class=\"index-alphabet\">" ;
    pf "<ul class=\"index-alphabet\">"; 
    List.iter ( fun (letter,l) ->
      pf "<li>%c</li>\n" letter ;
      pf "<ul  class=\"index-auteur\">" ;
      List.iter ( fun (auteur,songs) ->
	pf "<li>%s</li>\n" (String.capitalize  auteur) ;
	pf "<p  class=\"index-chanson\">" ; 
	let songs = List.sort ~cmp:compare songs in
	  List.iteri ( fun index s ->
	    if auteur <> s.Song.auteur then __SONG__failwith "internal error" else () ;
	    pf "<a id='song-%s'></a><a href='/view.songx?path=%s&output=all'>%s</a></br>" 
	      s.Song.filename 
	      s.Song.filename
	      s.Song.title ;
	    (*
	    List.iter ( fun o ->
	      (* pf "<li class=\"index-title\"><a href=\"%s/%s.html\"><span class=\"index-title\">%s</span></a></li>\n" *)
	      let clean_filename =  Str.global_replace (Str.regexp (Str.quote (s.Song.filename^"-"))) "" o.Output.filename in
		(* log "clean_filename = '%s'" clean_filename ; *)
		pf "<a href=\"%s%s/%s.html\"><span class=\"index-title\">(%s)</span></a>\n" 
		  root_path relative_output_dir o.Output.filename  
		  clean_filename
	    ) s.Song.outputs ;
	    *)
	    (* 
	       pf "<a href=\"%s.midi\"><span class=\"index-title\">(midi)</span></a>" s.Song.filename ;
	       pf "<a href=\"%s.wav\"><span class=\"index-title\">(wav)</span></a>" s.Song.filename ;
	       pf "<a href=\"%s.mp3\"><span class=\"index-title\">(mp3)</span></a>" s.Song.filename ;
	       pf "<a href=\"%s.pdf\"><span class=\"index-title\">(pdf)</span></a>" s.Song.filename ;
	       pf "<br/>" ;
	       pf "<a href=\"/edit_song.songx?index=%d\"><span class=\"index-title\">(edit)</span></a>" index ;
	    *)
	  ) songs ;
	  
	  pf "</p>"; 
      ) l ;
      pf "</ul>" ;
    ) songs_alphabet ;
    pf "</ul>"  ;
    ()
)
