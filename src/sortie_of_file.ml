open Data
open Printf
open ExtString
open Util

let pos_or_none pos = match pos with
  | None -> { Output.top=None;left=None;width=None;height=None }
  | Some pos -> pos

let make_top pos s = Some { (pos_or_none pos) with Output.top = Some (int_of_string s) }
let make_left pos s = Some { (pos_or_none pos) with Output.left = Some (int_of_string s) }
let make_width pos s = Some { (pos_or_none pos) with Output.width = Some (int_of_string s) }
let make_height pos s = Some { (pos_or_none pos) with Output.height = Some (int_of_string s) }

let parse_text s current = __SONG__try "parse text" (
  if Str.string_match (Str.regexp " *$") s 0 then current else (
    let (key,data) = __SONG__try "parse s" (
      if Str.string_match (Str.regexp "\\([^ ]+\\) +\\(.*\\)") s 0 then (
	let key = __SONG__try "match key ?" ( Str.matched_group 1 s) in
	let data = __SONG__try "match data ?" ( Str.matched_group 2 s) in
	  key,data
      ) else (
	__SONG__NOT_IMPLEMENTED__
      )
    ) in
    let current = match key with
      | "\\filename"  -> { current with Output.filename=data }

      | "\\lyrics"  -> { current with Output.lyrics=Some (pos_or_none current.Output.lyrics) }
      | "\\lyrics-top"  -> { current with Output.lyrics=make_top current.Output.lyrics data }
      | "\\lyrics-left"  -> { current with Output.lyrics=make_left current.Output.lyrics data }
      | "\\lyrics-width"  -> { current with Output.lyrics=make_width current.Output.lyrics data }
      | "\\lyrics-height"  -> { current with Output.lyrics=make_height current.Output.lyrics data }

      | "\\grille"  -> { current with Output.grille=Some(pos_or_none current.Output.grille) }
      | "\\grille-top"  -> { current with Output.grille=make_top current.Output.grille data }
      | "\\grille-left"  -> { current with Output.grille=make_left current.Output.grille data }
      | "\\grille-width"  -> { current with Output.grille=make_width current.Output.grille data }
      | "\\grille-height"  -> { current with Output.grille=make_height current.Output.grille data }

      | "\\structure"  -> { current with Output.structure=Some(pos_or_none current.Output.structure) }
      | "\\structure-top"  -> { current with Output.structure=make_top current.Output.structure data }
      | "\\structure-left"  -> { current with Output.structure=make_left current.Output.structure data }
      | "\\structure-width"  -> { current with Output.structure=make_width current.Output.structure data }
      | "\\structure-height"  -> { current with Output.structure=make_height current.Output.structure data }
      | s -> __SONG__failwith ("unknown keyword : '" ^ s ^ "'")
    in
      current
  )
)

let read_file song filename = __SONG__try "read_file" (
  let song = { song with Song.digest = Util.update_digest song.Song.digest filename } in
  let fin = open_in_bin filename in
  let rec read acc current linecount =
    try
      let line  = input_line fin in
	match current,line with
	  | None,"" -> read acc None (linecount+1)
	  | None,line -> let current = Some { Output.filename=line ; lyrics=None;grille=None;structure=None} in read acc current (linecount+1)
	  | Some current,"" -> read (current::acc) None (linecount+1)
	  | Some current,text ->let current = parse_text text current in read acc (Some current) (linecount+1)
    with
      | End_of_file -> close_in fin ; 
	  match current with
	    | None -> List.rev acc
	    | Some current -> List.rev (current::acc)
  in
  let data = read [] None 1 in
    (*
      List.iter ( fun l ->
      printf "------------> %s\n" l.Data.Lyrics.name ;
      printf "-->\n%s\n" l.Data.Lyrics.text
      ) data ;
    *)
    { song with Song.outputs = data }
)
