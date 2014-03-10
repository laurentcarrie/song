open Printf
open ExtString
open ExtList
open Util

module D = Data

let update_data set_list data = __SONG__try "update_data" (
  let data = Str.split (Str.regexp "\n") data in
  let (s,data) =  
    match data with
      | [] -> (__SONG__failwith("pas de donnees, il faut au moins le titre en premiere ligne") )
      | title::tl -> ( { set_list with D.Set_list.title=title ; paths=[]},tl )
  in

  let rec r s data =
    match data with 
      | [] -> s
      | hd::tl -> (
	  let s = { s with D.Set_list.paths = s.D.Set_list.paths @ (Str.split (Str.regexp " ")hd) } in
	    r s tl
	)
  in
    (* TODO : verifier qu'elles existent *)
    r s data
)

let read_file set_list filename = __SONG__try "read_file" (
  let data = Std.input_file filename in
  let set_list = { set_list with D.Set_list.path = filename } in
    update_data set_list data
)


let to_print print sl = __SONG__try "to_string" (
  let pf fs = ksprintf print fs in
    pf "%s\n" sl.D.Set_list.title ;
    List.iter ( fun s -> 
      pf "%s\n" s 
    ) sl.D.Set_list.paths
)

let to_string sl = __SONG__try "to_string" (
  let buf = Buffer.create 1024 in
  let () = to_print (Buffer.add_string buf) sl  in
    Buffer.contents buf
)


let to_html_print print sl = (
  let pf fs = ksprintf print fs in
    pf "<h2>%s</h2>\n" sl.D.Set_list.title ;
    pf "<ul>\n" ;
    List.iter ( fun s ->
      pf "%s\n" s
    ) sl.D.Set_list.paths ;
    pf "</ul>\n" 
)


let to_html sl = __SONG__try "to_html" (
  let buf = Buffer.create 1024 in
  let () = to_html_print (Buffer.add_string buf) sl  in
    Buffer.contents buf
)



let read world = __SONG__try "read" (
  
)
  
let to_string paths = 
  List.fold_left ( fun acc s -> acc ^ s ^ "\n") "" paths



let write_index print head world onoff = __SONG__try "write index" (
  let pfnl fs = ksprintf ( fun s -> print s ; print "\n")  fs in
  let () = match onoff with
      | On_line -> (
	  List.iter ( fun sl ->
	    Edit_type.print ~loadurl:"/data-songlist.songx" print "/internal-edit-setlist.songx" sl.D.Set_list.path "edit-title" "title" Edit_type.Text ;
	    Edit_type.print ~loadurl:"/data-songlist.songx" print "/internal-edit-setlist.songx" sl.D.Set_list.path "edit-paths" "paths" 	Edit_type.Textarea ;
	  ) world.D.World.set_lists ;
	)
      | Off_line -> ()
  in
    head () ;
    pfnl "%d setlists" ( List.length world.D.World.set_lists) ;
    pfnl "<ul>" ;
    List.iter ( fun sl ->
      let first = match sl.D.Set_list.paths with
	| [] -> "setlist vide"
	| hd::_ -> hd
      in
	pfnl "<li>" ;
	(match onoff with
	  | On_line -> (
	      pfnl "<a href='/view.songx?path=%s&setlist=%s&output=all'>%s</a>" 
		first  (strip_root world sl.D.Set_list.path) sl.D.Set_list.title ;
	      pfnl "<div class='edit edit-title' id='edit-title'>titre</div>" ;
	      pfnl "<div class='edit edit-paths' id='edit-paths'>chansons</div>" ;
	    )
	  | Off_line -> (
	      pfnl "<a href='%s/all.html'>%s</a>" 
		first sl.D.Set_list.title ;
	    )) ;
	pfnl "</li>" ;
    ) world.D.World.set_lists ;
    pfnl "</ul>" ;
)
      
