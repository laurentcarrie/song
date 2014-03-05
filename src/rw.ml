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
	  
  type r = 
      | Normal
      | Zone of (t * string)
end

open Read

let log = Fcgi.log

let from_file filename = __SONG__try ("from file " ^ filename) (
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
	    | Normal,"" -> split acc Normal
	    | Normal,line -> log "ligne hors zone : %s"  line ; split acc Normal

	    | Zone (Read.Info,i)  ,"=== END INFO ==="   -> split ((Info,i)::acc) Normal
	    | Zone (Read.Lyrics,i),"=== END LYRICS ==="   -> split ((Lyrics,i)::acc) Normal
	    | Zone (Read.Grille,i),"=== END GRILLE ==="   -> split ((Grille,i)::acc) Normal
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
    tempo = 80 ;
    server_path = "xxxxxxxxxxxxxxx" ;
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
	col_2 = [ D.Output.G ; D.Output.S ] ;
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
  let fout = open_out_bin song.D.Song.path in
  let print s = fprintf fout "%s\n" s in
  let pf fs = ksprintf print fs in
    pf "=== BEGIN LYRICS ===" ;
    Lyrics.to_print print song ;
    pf "=== END LYRICS ===" ;
)
  
let find_all_songs root = __SONG__try "find all songs" (
  let rec find acc root =
    let dirs = Sys.readdir root in
    let dirs = Array.to_list dirs in
      List.fold_left ( fun acc d ->
	let d = root//d in
	let () = log "Reading directory %s" d in
	let acc = (
	  if  Sys.is_directory d then (
	    find acc d
	  ) else (
	    if Filename.check_suffix d ".song" then  d :: acc  else acc
	  )
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
	(from_file path)::songs,errors
      with
	| e -> 
	    let msg = Song_exn.string_of_stack () in
	    let ()  = Song_exn.clear_stack () in
	    let () = log "ERROR : %s" msg in
	      songs,path::errors
    ) ([],[]) paths
)
