open Printf
open ExtString

module D = Data

let to_print print song = __SONG__try "to_string" (
  let pf fs = ksprintf print fs in
    List.iter ( fun s ->
      pf "%s\n" s.D.Structure.section_name ;
      pf "%s\n" s.D.Structure.comment ;
    ) song.D.Song.structure
)

let update_data song data = __SONG__try "update_data" (
  let data = Str.split (Str.regexp "\n") data in
  let rec read acc current data linecount =
    match data with
      | [] -> (
	  match current with
	    | None -> List.rev acc
	    | Some current -> List.rev (current::acc)
	)
      | line::tl -> (
	  let line  = String.strip line in
	    match current,line with
	      | None,"" -> read acc None tl (linecount+1)
	      | None,line -> let current = Some { D.Structure.section_name=line ; comment="";} in read acc current tl (linecount+1)
	      | Some current,"" -> read (current::acc) None tl (linecount+1)
	      | Some current,text ->let current = { current with D.Structure.comment=current.D.Structure.comment^line^"\n" } in 
				      read acc (Some current) tl (linecount+1)
	)
  in
  let data = read [] None data 1 in
    { song with D.Song.structure=data }
)

