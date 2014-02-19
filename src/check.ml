open Data
open Printf

let check song = __SONG__try "check" (
  List.iter ( fun s ->
		try  
		  let _ = PMap.find s.Structure.section_name song.Song.sections in ()
		with
		  | Not_found ->
		      let msg = sprintf "Dans la structure, la section '%s' n'existe pas dans la grille" s.Structure.section_name in
			__SONG__failwith msg
	    ) song.Song.structure ;
  song
)
