open Data
open Printf
open ExtString


let int_of_string s =
  try
    int_of_string s
  with
    | e -> __SONG__failwith("cannot convert to int : '" ^ s ^"'")


type check = {
  has_filename : bool ;
  has_title : bool ;
  has_auteur : bool ;
}

let read_file song filename = __SONG__try "read_file" (
  let check = { has_filename=false;has_title=false;has_auteur=false}  in
  let fin = open_in_bin filename in
  let rec read (song,check) linecount =
    try
      let line  = input_line fin in
	if Str.string_match (Str.regexp " *$") line 0 then read (song,check) (linecount+1) else (
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
	    | "\\titre" | "\\title"   -> { song with Song.title=String.strip data },{ check with has_title=true }
	    | "\\auteur" | "\\author" -> { song with Song.auteur=String.strip data },{ check with has_auteur=true }
	    | "\\tempo"  -> { song with Song.tempo=int_of_string (String.strip data) },check
	    | "\\filename"  -> { song with Song.filename=String.strip data } , { check with has_filename = true }
	    | s -> __SONG__failwith ("unknown keyword : '" ^ s ^ "'")
	  in
	    read (song,check) (linecount+1)
	)
    with
      | End_of_file -> close_in fin ; (song,check)
  in
  let (song,check) = read (song,check) 1 in
  let () = if not check.has_title then __SONG__failwith ("\\titre non défini") else () in
  let () = if not check.has_filename then __SONG__failwith ("\\filename non défini") else () in
  let () = if not check.has_auteur then __SONG__failwith ("\\auteur non défini") else () in
    song
)
