open Data
open Printf
open ExtString
open Util


let parse_col s col current = __SONG__try "parse col" (
  List.fold_left ( fun current s ->
    match col,s with
      | 1,"\\lyrics" ->  { current with Output.col_1=current.Output.col_1 @ [Output.L] }
      | 2,"\\lyrics" ->  { current with Output.col_2=current.Output.col_2 @ [Output.L] }
      | 1,"\\grille" ->  { current with Output.col_1=current.Output.col_1 @ [Output.G] }
      | 2,"\\grille" ->  { current with Output.col_2=current.Output.col_2 @ [Output.G] }
      | 1,"\\structure" ->  { current with Output.col_1=current.Output.col_1 @ [Output.S] }
      | 2,"\\structure" ->  { current with Output.col_2=current.Output.col_2 @ [Output.S] }
      | 1,s
      | 2,s -> __SONG__failwith ("mauvais mot clef : " ^ s)
      | i,_ -> __SONG__failwith ("mauvais index de colonne : " ^ (string_of_int i))
  ) current (Str.split (Str.regexp " ") s) 
)

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
      | "\\col-1" -> parse_col data 1 current
      | "\\col-2" -> parse_col data 2 current

      | s -> __SONG__failwith ("unknown keyword : '" ^ s ^ "'")
    in
      current
  )
)

let read_file song filename = __SONG__try ("read_file ") (
  let song = { song with Song.digest = Util.update_digest song.Song.digest filename } in
  let song = 
    if Sys.file_exists filename then (
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
    else (
	song
    )
  in

  let std_outputs = [
    {
      Output.filename = sprintf "%s-all" song.Song.filename ;
      col_1 = [ Output.L ; ]  ;
      col_2 = [ Output.G ; Output.S ] ;
      width = 25 ;
    } ; 
    {
      Output.filename = sprintf "%s-grille" song.Song.filename ;
      col_1 = [ Output.G ; ]  ;
      col_2 = [ Output.S ] ;
      width = 25 ;
    } ; 
  ] in
  let song = { song with Song.outputs = song.Song.outputs @ std_outputs } in
    song

)
