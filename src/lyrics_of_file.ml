open Data
open Printf
open ExtString

let read_data song data = __SONG__try "read_file" (
  let data = Str.split (Str.regexp "\n") data in
  let rec read acc current linecount data =
    match data with
      | [] -> (
    	  match current with
	    | None -> List.rev acc
	    | Some current -> List.rev (current::acc)
	)
      | line::tl -> (
	  match current,line with
	    | None,"" -> read acc None (linecount+1) tl
	    | None,line -> let current = Some { Lyrics.name=line ; text="";index=None} in read acc current (linecount+1) tl
	    | Some current,"" -> read (current::acc) None (linecount+1) tl
	    | Some current,text ->let current = { current with Data.Lyrics.text=current.Data.Lyrics.text^line^"\n" } in read acc (Some current) (linecount+1) tl
	)
  in
  let data = read [] None 1 data in
    { song with Data.Song.lyrics = data }
)

let read_file song filename = __SONG__try "read_file" (
  let data = Std.input_file filename in
    read_data song data
)
