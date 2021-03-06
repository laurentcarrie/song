open Util
open Printf
open ExtString
open ExtList
module D = Data

module Read = struct
  type t = 
      | Info
      | Lyrics
      | Grille
      | Structure
      | Lilypond
	  
  type r = 
      | Normal
      | Zone of (t * string)
end

open Read

let log = Fcgi.log

let song_of_file filename = __SONG__try ("from file " ^ filename) (
  let split filename =
    let fin = open_in_bin filename in
    let rec split acc status = 
      try
	let line = input_line fin in
	let line = String.strip line in
	  match status,String.strip line  with

	    | Normal,"=== BEGIN INFO ==="   -> split acc (Zone(Info,""))
	    | Normal,"=== BEGIN LYRICS ==="   -> split acc (Zone(Lyrics,""))
	    | Normal,"=== BEGIN GRILLE ==="   -> split acc (Zone(Grille,""))
	    | Normal,"=== BEGIN STRUCTURE ==="   -> split acc (Zone(Structure,""))
	    | Normal,"=== BEGIN LILYPONDS ==="   -> split acc (Zone(Lilypond,""))
	    | Normal,"" -> split acc Normal
	    | Normal,line -> log "ligne hors zone : %s"  line ; split acc Normal

	    | Zone (Read.Info,i)  ,"=== END INFO ==="   -> split ((Info,i)::acc) Normal
	    | Zone (Read.Lyrics,i),"=== END LYRICS ==="   -> split ((Lyrics,i)::acc) Normal
	    | Zone (Read.Grille,i),"=== END GRILLE ==="   -> split ((Grille,i)::acc) Normal
	    | Zone (Read.Lilypond,i),"=== END LILYPONDS ==="   -> split ((Lilypond,i)::acc) Normal
	    | Zone (Read.Structure,i),"=== END STRUCTURE ==="   -> split ((Structure,i)::acc) Normal
	    | Zone (z,i),line ->             split acc (Zone (z,i^"\n"^line))
      with
	| End_of_file ->
	    close_in fin ;
	    acc
    in

    let zones = split [] Normal in
      zones
  in
  let zones = split filename in
  let song = {
    D.Song.title = "xxxxxxxxxxx" ;
    auteur = "xxxxxxxxxxxxxxx" ;
    format = None ;
    sections = [] ;
    structure = [] ;
    lyrics = [] ;
    outputs = [] ;
    lilyponds = [] ;
    tempo = 80 ;
    path = filename 
  } in

  let find (z:Read.t) = 
    let acc = List.fold_left ( fun acc (t,data) -> 
      if t = z then 
	match acc with
	  | None -> Some data
	  | Some s -> Some (s^data)
      else 
	acc
    ) None zones 
    in
      acc
  in
  let find_or_default z = match find z with | None -> "" | Some s -> s in
  let song = Lyrics.update_data song (find_or_default Lyrics) in
  let song = Grille.update_data song (find_or_default Grille) in
  let song = Lilypond.update_data song (find_or_default Lilypond) in
  let song = 
    match find Info with
      | None -> __SONG__failwith "section info non trouvée, ( === BEGIN INFO === ..... === END INFO === )" 
      | Some info ->  Info.update_data song  info
  in
  let song = Structure.update_data song (find_or_default Structure) in
  let song = 
    let std_outputs = [
      {
	(* Output.filename = sprintf "%s-all" song.Song.filename ; *)
	D.Output.filename = "all" ;
	col_1 = [ D.Output.L ; ]  ;
	col_2 = [ D.Output.G ; D.Output.S ; D.Output.Lily ] ;
	width = 25 ;
      } ; 
      {
	D.Output.filename = "grille" ;
	col_1 = [ D.Output.G ; ]  ;
	col_2 = [ D.Output.S ] ;
	width = 25 ;
      } ; 
    ] in
      { song with D.Song.outputs = std_outputs }
  in
    song
)

let write_song song = __SONG__try "write" (
  let () = if Filename.is_relative song.D.Song.path then
    __SONG__failwith ("cannot write to relative path : " ^ song.D.Song.path)
  else ()
  in
  let () = 
    let d = Filename.dirname song.D.Song.path in
      mkdir d
  in
  log "writing song to %s" song.D.Song.path ;
  let fout = open_out_bin song.D.Song.path in
  let print s = fprintf fout "%s" s in
  (* let pf fs = ksprintf print fs in *)
  let pfnl fs = ksprintf ( fun s -> print s ; print "\n") fs in
    pfnl "=== BEGIN INFO ===" ;
    Info.to_print print song ;
    pfnl "=== END INFO ===" ;

    pfnl "=== BEGIN LYRICS ===" ;
    Lyrics.to_print print song ;
    pfnl "=== END LYRICS ===" ;

    pfnl "=== BEGIN GRILLE ===" ;
    Grille.to_print print song ;
    pfnl "=== END GRILLE ===" ;

    pfnl "=== BEGIN STRUCTURE ===" ;
    Structure.to_print print song ;
    pfnl "=== END STRUCTURE ===" ;

    pfnl "=== BEGIN LILYPONDS ===" ;
    Lilypond.to_print print song ;
    pfnl "=== END LILYPONDS ===" ;

    close_out fout
)
  
let find_all_songs root = __SONG__try "find all songs" (
  let rec find acc root =
    let dirs = __SONG__try "readdir" (Sys.readdir root) in
    let dirs = __SONG__try "to list" ( Array.to_list dirs) in
      List.fold_left ( fun acc d ->
	let d = root//d in
	let () = log "Reading directory %s" d in
	let acc = (
	  try
	    if  __SONG__try "is_directory" (Sys.is_directory d) then (
	      find acc d
	    ) else (
	      if __SONG__try "check_suffix" (Filename.check_suffix d ".song") then  d :: acc  else acc
	    )
	  with
	    | Sys_error e -> log "error : %s" e ; acc
	) in
	  acc
      ) acc dirs 
  in
    find [] root
)

let all_songs_from_root root =  __SONG__try "all_songs_from_root" (
  log "all_songs_from_root"  ;
  let paths = find_all_songs root in
    List.fold_left ( fun (songs,errors) path ->
      try
	(song_of_file path)::songs,errors
      with
	| e -> 
	    let msg = Song_exn.string_of_stack e in
	    let ()  = Song_exn.clear_stack () in
	    let () = log "ERROR : %s" msg in
	      songs,path::errors
    ) ([],[]) paths
)

let find_all_sls root = __SONG__try "find all sl" (
  let rec find acc root =
    let dirs = __SONG__try "readdir" (Sys.readdir root) in
    let dirs = __SONG__try "to list" ( Array.to_list dirs) in
      List.fold_left ( fun acc d ->
	let d = root//d in
	let () = log "Reading directory %s" d in
	let acc = (
	  try
	    if  __SONG__try "is_directory" (Sys.is_directory d) then (
	      find acc d
	    ) else (
	      if __SONG__try "check_suffix" (Filename.check_suffix d ".setlist") then  d :: acc  else acc
	    )
	  with
	    | Sys_error e -> log "error : %s" e ; acc
	) in
	  acc
      ) acc dirs 
  in
    find [] root
)

let all_sls_from_root root =  __SONG__try "all_sl_from_root" (
  let paths = find_all_sls root in
    let (sls,_) = List.fold_left ( fun (sls,errors) path ->
      try
	let s = { D.Set_list.title="";paths=[];path=path } in
	let s = Set_list.read_file s path in
	  s::sls,errors
      with
	| e -> 
	    let msg = Song_exn.string_of_stack e in
	    let ()  = Song_exn.clear_stack () in
	    let () = log "ERROR : %s" msg in
	      sls,path::errors
    ) ([],[]) paths in
      sls
)


let write_setlist sl = __SONG__try "write" (
  let () = if Filename.is_relative sl.D.Set_list.path then
    __SONG__failwith ("cannot write to relative path : " ^ sl.D.Set_list.path)
  else ()
  in
  let () = 
    let d = Filename.dirname sl.D.Set_list.path in
      mkdir d
  in
  log "writing sl to %s" sl.D.Set_list.path ;
  let fout = open_out_bin sl.D.Set_list.path in
  let print s = fprintf fout "%s" s in
  (* let pf fs = ksprintf print fs in *)
  let pfnl fs = ksprintf ( fun s -> print s ; print "\n") fs in
    pfnl "%s" sl.D.Set_list.title ;
    List.iter ( fun p ->
      pfnl "%s" p
    ) sl.D.Set_list.paths ;
    close_out fout
)
