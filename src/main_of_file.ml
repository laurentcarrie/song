open Data
open Printf
open ExtString




let read_file song filename = __SONG__try "read_file" (
  let song = { song with Song.digest = Util.update_digest song.Song.digest filename } in
  let fin = open_in_bin filename in
  let rec read song linecount =
    try
      let line  = input_line fin in
	if Str.string_match (Str.regexp " *$") line 0 then read song (linecount+1) else (
	  let (key,data) = __SONG__try "parse line" (
	    if Str.string_match (Str.regexp "\\([^ ]+\\)\\(.*\\)") line 0 then (
	      let key = __SONG__try "match key ?" ( Str.matched_group 1 line) in
	      let data = __SONG__try "match data ?" ( Str.matched_group 2 line) in
		key,data
	    ) else (
	      __SONG__NOT_IMPLEMENTED__
	    )
	  ) in

	  let song = match key with
	    | "\\titre" | "\\title"   -> { song with Song.title=String.strip data }
	    | "\\auteur" | "\\author" -> { song with Song.auteur=String.strip data }
	    | s -> __SONG__failwith ("unknown keyword : '" ^ s ^ "'")
	  in
	    read song (linecount+1)
	)
    with
      | End_of_file -> close_in fin ; song
  in
  let song = read song 1 in
    song
)
