open Util
open Data
open Printf
open ExtList
let (//) = Filename.concat

let compare song1 song2 =
  match String.compare song1.Song.auteur song2.Song.auteur with
    | 0 -> String.compare song1.Song.title song2.Song.title
      | i -> i

let write_index ~songs ~output_dir ~relative_output_dir = __SONG__try "index" (
  let b = Buffer.create 1024 in
  let pf fs = ksprintf (Buffer.add_string b) fs in 
  let songs = List.fold_left ( fun songs s ->
				 let auteur = s.Song.auteur in
				   try
				     let l = List.assoc auteur songs in
				       (auteur,(s::l)) :: (List.remove_assoc auteur songs)
				   with
				     | Not_found -> (s.Song.auteur,[s]) :: songs
			     ) [] songs
  in
    
    pf "<html>" ;
    pf "<style>
.index-title {
left: 1cm ;
font-size: 1.em ;
}
.index-auteur {
font-size: 1.em ;
}
</style>
" ;
    pf "<body>\n" ;
    let compare (a1,_) (a2,_) = String.compare a1 a2 in
    let songs = List.sort ~cmp:compare songs in
      List.iter ( fun (auteur,l) ->
		    pf "<p class=\"index-auteur\">%s</p>\n" auteur ;
		    List.iter ( fun s ->
				  if auteur <> s.Song.auteur then __SONG__failwith "internal error" else () ;
				  List.iter ( fun o ->
						pf "<p class=\"index\"><a href=\"%s/%s.html\"><span class=\"index-title\">%s</span></a></p>\n"
						  relative_output_dir o.Output.filename  s.Song.title
					    ) s.Song.outputs ;
			      ) l ;
		) songs ;
      pf "</body>\n" ;
      pf "</html>\n" ;
      Std.output_file ~filename:(output_dir // "index.html") ~text:(Buffer.contents b) ;
      ()
	

)
