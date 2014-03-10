open Util
open Printf
open ExtList

module D = Data

let (//) = Filename.concat

let log = Fcgi.log

let alphabet = 
  let c1 = Char.code 'A' in
  let c2 = Char.code 'Z' in
  let rec build acc current = 
    if current>c2 then List.rev ('*'::acc)
    else build ((Char.chr current)::acc) (current+1)
  in
    build [] c1
      
let is_letter c = 
  let c1 = Char.code 'A' in
  let c2 = Char.code 'Z' in
  let c =  Char.code (Char.uppercase c) in
    (c1<=c) && (c<=c2)
    
let compare song1 song2 =
  match String.compare song1.D.Song.auteur song2.D.Song.auteur with
    | 0 -> String.compare song1.D.Song.title song2.D.Song.title
    | i -> i

let write_index print world onoff = __SONG__try "index" (
  log "write index"; 
  let songs = world.D.World.songs in
  let pf fs = ksprintf ( fun s -> print s ; print "\n" ) fs in
  let songs_assoc = List.fold_left ( fun acc s ->
    let auteur = s.D.Song.auteur in
      try
	let l = List.assoc auteur acc in
	  (auteur,(s::l)) :: (List.remove_assoc auteur acc)
      with
	| Not_found -> (s.D.Song.auteur,[s]) :: acc
  ) [] songs
  in
  let songs_alphabet = List.map ( fun letter ->
    (* log "index, examen lettre %c" letter ; *)
    let acc = List.fold_left ( fun acc (auteur,songs) ->
      (* log "... auteur = '%s'" auteur ; *)
      let () = if auteur = "" then __SONG__failwith ("auteur non defini") else () in
      let l2 = Char.uppercase (String.get auteur 0) in
	if is_letter l2 then (
	  if letter = l2 then (
	    (* log "... match\n" ;  *)
	    (auteur,songs)::acc
	  )
	  else (
	    (* log "don't match '%c' '%c'" letter l2 ;  *)
	    acc
	  )
	) else (
	  if letter = '*' then
	    (auteur,songs)::acc
	  else
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
    
    pf "<a name='top' />" ;
    let () = match onoff with
      | On_line -> (
	  pf "<a href='/download.songx'>Télécharger sous forme d'un fichier zip</a><br/>"  ;
	  pf "<a href='/reload.songx'>Recharger à partir du disque</a><br/>"  ;
	  pf "<a href='/new.songx'>Créer un nouveau morceau</a><br/>"  ;
	  pf "<a href='/setlists.songx'>Set lists</a><br/>"  ;
	  if List.length world.D.World.errors > 0 then (
	    pf "<a href='/errors.songx'>%d errors</a>"  (List.length world.D.World.errors) ;
	  ) else ()
	)
      | Off_line -> (
	  let tm = Unix.localtime(Unix.time ()) in
	    pf "Généré le  %02d/%02d/%04d à  %02d:%02d:%02d</br>" 
	      tm.Unix.tm_mday (tm.Unix.tm_mon+1) (tm.Unix.tm_year+1900)
	      tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec ;
	    pf "<a href='setlists.html'>Set lists</a><br/>"  ;
	) 
    in

    pf "<div id=\"list5\" class=\"index-alphabet\">" ;
    pf "<ul class=\"index-alphabet\">"; 
    List.iter ( fun (letter,l) ->
      pf "<li><a href='#top'>%c</a></li>\n" letter ;
      pf "<ul  class=\"index-auteur\">" ;
      let l = List.sort ~cmp:( fun (a1,_) (a2,_) -> String.compare a1 a2) l in
      List.iter ( fun (auteur,songs) ->
	pf "<li>%s</li>\n" (String.capitalize  auteur) ;
	pf "<p  class=\"index-chanson\">" ; 
	let songs = List.sort ~cmp:compare songs in
	  List.iteri ( fun index s ->
	    if auteur <> s.D.Song.auteur then __SONG__failwith "internal error" else () ;
	    let filename = strip_root world s.D.Song.path in
	    ( match onoff with
	      | On_line ->  pf "<a id='song-%s'></a><a href='/view.songx?path=%s&output=all'>%s</a></br>" 
		  filename 
		    filename
		    s.D.Song.title 
	      | Off_line ->  pf "<a id='song-%s'></a><a href='%s/all.html'>%s</a></br>" 
		  filename 
		    filename
		    s.D.Song.title 
	    ) ;
	    (* 
	       pf "<a href=\"%s.midi\"><span class=\"index-title\">(midi)</span></a>" s.D.Song.filename ;
	       pf "<a href=\"%s.wav\"><span class=\"index-title\">(wav)</span></a>" s.D.Song.filename ;
	       pf "<a href=\"%s.mp3\"><span class=\"index-title\">(mp3)</span></a>" s.D.Song.filename ;
	       pf "<a href=\"%s.pdf\"><span class=\"index-title\">(pdf)</span></a>" s.D.Song.filename ;
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
