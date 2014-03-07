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

let length_of_section s =
  List.fold_left ( fun acc c -> acc + 
    match c with
      | D.Section.NL -> 0
      | D.Section.C c -> c.D.Chord.length 
      | D.Section.G _ -> 4
  ) 0 s.D.Section.chords

let to_html_print print song = __SONG__try "to_html_print" (
  let pf fs = ksprintf print fs in
    pf "<h2>structure</h2>\n" ;
    pf "<ol class=\"structure-list\">\n" ;
    let count = ref 1 in
      List.iter ( fun s ->
	let sname = s.D.Structure.section_name in
	let s = try List.find ( fun current -> current.D.Section.name = sname) song.D.Song.sections with
	  | Not_found -> __SONG__failwith ("pas de structure nommee " ^ sname )in
	let l = length_of_section s in
	let count2 = !count + l in 
	  (* pf "<li class=\"structure-list\">%s  (%d : %d &rarr; %d) </li>\n" s.D.Section.name l (!count/4+1) (count2/4) ; *)
	  pf "<li class=\"structure-list\">%s</li>\n" s.D.Section.name ; 
	  count := count2  ;
      ) song.D.Song.structure ;
      pf "</ol>\n" ;
)

