open Printf
open ExtString

module D = Data

let int_of_string s =
  try
    int_of_string s
  with
    | e -> __SONG__failwith("cannot convert to int : '" ^ s ^"'")


type check = {
  has_title : bool ;
  has_auteur : bool ;
}

let to_print print song = __SONG__try "to_string" (
  let pf fs = ksprintf print fs in
    pf "\\titre %s\n" song.D.Song.title ;
    pf "\\auteur %s\n" song.D.Song.auteur ;
    pf "\\tempo %d\n" song.D.Song.tempo ;
)

let update_data song data = __SONG__try "update_data" (
  let check = { has_title=false;has_auteur=false}  in

  let rec read (song,check) data linecount =
    match data with
      | [] -> song,check
      | line::tl -> (
	  if Str.string_match (Str.regexp " *$") line 0 then read (song,check) tl (linecount+1) else (
	    let (key,data) = __SONG__try "parse line" (
	      if Str.string_match (Str.regexp "\\([^ ]+\\)\\(.*\\)") line 0 then (
		let key = __SONG__try "match key ?" ( Str.matched_group 1 line) in
		let data = __SONG__try "match data ?" ( Str.matched_group 2 line) in
		  key,data
	      ) else (
		__SONG__NOT_IMPLEMENTED__
	      )
	    ) in
	      
	    let song,check = match key with
	      | "\\titre" | "\\title"   -> { song with D.Song.title=String.strip data },{ check with has_title=true }
	      | "\\auteur" | "\\author" -> { song with D.Song.auteur=String.strip data },{ check with has_auteur=true }
	      | "\\tempo"  -> { song with D.Song.tempo=int_of_string (String.strip data) },check
	      | s -> __SONG__failwith ("unknown keyword : '" ^ s ^ "'")
	    in
	      read (song,check) tl (linecount+1)
	  )
	)
  in
  let (song,check) = read (song,check) (Str.split (Str.regexp "\n") data) 1 in
  let () = if not check.has_title then __SONG__failwith ("\\titre non défini") else () in
  let () = if not check.has_auteur then __SONG__failwith ("\\auteur non défini") else () in
    song
)
