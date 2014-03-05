open Printf
open ExtString

module D = Data

let read_file song filename = __SONG__try "read_file" (
  let fin = open_in_bin filename in
  let rec read acc current linecount =
    try
      let line  = String.strip (input_line fin) in
	match current,line with
	  | None,"" -> read acc None (linecount+1)
	  | None,line -> let current = Some { D.Structure.section_name=line ; comment="";} in read acc current (linecount+1)
	  | Some current,"" -> read (current::acc) None (linecount+1)
	  | Some current,text ->let current = { current with D.Structure.comment=current.D.Structure.comment^line^"\n" } in read acc (Some current) (linecount+1)
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
    { song with D.Song.structure=data }
)
