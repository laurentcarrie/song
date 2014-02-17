open Data
open Printf
open ExtString
open Util

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
      | "\\width"  -> { current with Output.width=__SONG__try "width" (int_of_string data) }

      | "\\lyrics-col-1"  -> { current with Output.col_1=current.Output.col_1 @ [Output.L] }
      | "\\lyrics-col-2"  -> { current with Output.col_2=current.Output.col_2 @ [Output.L] }

      | "\\grille-col-1"  -> { current with Output.col_1=current.Output.col_1 @ [Output.G] }
      | "\\grille-col-2"  -> { current with Output.col_2=current.Output.col_2 @ [Output.G] }

      | "\\structure-col-1"  -> { current with Output.col_1=current.Output.col_1 @ [Output.S] }
      | "\\structure-col-2"  -> { current with Output.col_2=current.Output.col_2 @ [Output.S] }

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
	  | None,line -> let current = Some { Output.filename=line ; col_1=[];col_2=[];width=20} in read acc current (linecount+1)
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
