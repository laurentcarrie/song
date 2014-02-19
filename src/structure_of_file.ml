open Data
open Printf
open ExtString


let read_file song filename = __SONG__try "read_file" (
  let song = { song with Song.digest = Util.update_digest song.Song.digest filename } in
  let fin = open_in_bin filename in
  let rec read acc current linecount =
    try
      let line  = String.strip (input_line fin) in
	match current,line with
	  | None,"" -> read acc None (linecount+1)
	  | None,line -> let current = Some { Structure.section_name=line ; comment="";} in read acc current (linecount+1)
	  | Some current,"" -> read (current::acc) None (linecount+1)
	  | Some current,text ->let current = { current with Structure.comment=current.Structure.comment^line^"\n" } in read acc (Some current) (linecount+1)
    with
      | End_of_file -> close_in fin ; 
	  match current with
	    | None -> List.rev acc
	    | Some current -> List.rev (current::acc)
  in
  let data = read [] None 1 in
    (*
      List.iter ( fun l ->
      printf "structure ----------> %s\n" l.Structure.section_name ;
      printf "-->\n%s\n" l.Structure.comment
      ) data ;
    *)
    { song with Song.structure=data }
)
