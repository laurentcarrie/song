open Util
open Printf
open ExtString
open ExtList
open Data

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

let from_file filename = __SONG__try "from file" (
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
    Song.title = "xxxxxxxxxxx" ;
    auteur = "xxxxxxxxxxxxxxx" ;
    filename = "xxxxxxxxxxxxxx" ;
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
      if t = z then acc ^ data else acc
    ) "" zones 
    in
      acc
  in
  let song = Lyrics_of_file.update_data song (find Lyrics) in
    song
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

let all_songs_from_root root : Song.t list =  __SONG__try "all_songs_from_root" (
  log "all_songs_from_root"  ;
  let paths = find_all_songs root in
    List.map from_file paths
    
)
