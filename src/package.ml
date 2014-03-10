open Printf
open Util
open ExtList

module D = Data

let (//) = Filename.concat

let log = Fcgi.log

let make_zip world head print = __SONG__try "make_zip" (
  
  let pf fs = ksprintf print fs in


    let zf = __SONG__try "create zip file" (Zip.open_out (world.D.World.doc_root // "download" // "songs.zip")) in

      (
	let buf = Buffer.create 1024 in
	let print = Buffer.add_string buf in
	let pf fs = ksprintf print fs in
	  pf "
<hml>
<head>
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
<link rel=\"stylesheet\" href=\"song.css\"/>
";
	  let () = Index.write_index print world Off_line in
	    Zip.add_entry (Buffer.contents buf) zf "index.html"
      ) ;

      (
	let buf = Buffer.create 1024 in
	let print = Buffer.add_string buf in
	let pf fs = ksprintf print fs in
	  pf "
<hml>
<head>
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
<link rel=\"stylesheet\" href=\"song.css\"/>
";
	  let head () = () in
	  let () = Set_list.write_index print head world Off_line in
	    Zip.add_entry (Buffer.contents buf) zf "setlists.html"
      ) ;

      (
	let buf = Buffer.create 1024 in
	let print = Buffer.add_string buf in
	let () = Css.print_css print in
	  Zip.add_entry (Buffer.contents buf) zf "song.css"
      ) ;

      (
	(* images *)
	List.iter ( fun s ->
	  let filename = world.D.World.doc_root // "img" // s in
	    Zip.copy_file_to_entry filename zf (sprintf "img/%s" s)
	) [
	  "20140310105205472_easyicon_net_72.png" ;
	  "20140310105841100_easyicon_net_72.png" ;
	]
      ) ;


      List.iter ( fun song ->
	List.iter ( fun output ->
	  let buf = Buffer.create 1024 in
	  let print = Buffer.add_string buf in
	  let filename = strip_root world song.D.Song.path in
	  let depth = List.length (path_to_list ("/"^filename)) in
	  let path_to_css = 
	    let rec r path d =
	      if d=0 then path else r ("../"^path) (d-1)
	    in
	      r "song.css" depth
	  in
	  let pf fs = ksprintf print fs in
	    pf "
<html>
<head>
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
<link rel=\"stylesheet\" ";
	    pf "href=\"%s\"" path_to_css ;
	    pf "\"/>
" ;
	    let () = Html.render_output world head print song output Off_line None in
	    let data = Buffer.contents buf in
	    let filename = filename // (sprintf "%s.html" output.D.Output.filename) in
	      Zip.add_entry data zf filename
	) song.D.Song.outputs ;

	let base = strip_root world song.D.Song.path in
	List.iteri ( fun index lily ->
	  match lily.D.Lilypond.status with
	    | D.Lilypond.Ok filename -> (
		let filename = sprintf "%s/tmp/%s" world.D.World.doc_root filename in
		  try
		    Zip.copy_file_to_entry filename zf (sprintf "%s/%s" base (Filename.basename filename))
		  with
		    | e -> 
			let msg = Song_exn.string_of_stack e in
			let () = Song_exn.clear_stack () in
			let () = log "ERROR : %s" msg in
			  ()
	      )
	    | D.Lilypond.Error _ -> ()
	    | D.Lilypond.Unknown -> log "in package, UNKNOWN status found ????"
	) song.D.Song.lilyponds ;
	  
	  
      ) world.D.World.songs ;


      (* set lists *)
      (
	List.iter ( fun sl ->
	  let buf = Buffer.create 1024 in
	  let print = Buffer.add_string buf in
	  let filename = strip_root world sl.D.Set_list.path in
	  let depth = List.length (path_to_list ("/"^filename)) in
	  let path_to_css = 
	    let rec r path d =
	      if d=0 then path else r ("../"^path) (d-1)
	    in
	      r "song.css" (depth-1)
	  in
	  let pf fs = ksprintf print fs in
	    pf "
<html>
<head>
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
<link rel=\"stylesheet\" ";
	    pf "href=\"%s\"" path_to_css ;
	    pf "\"/>
</head>
<body>
" ;
	    pf "<ul>" ;
	    List.iter ( fun path ->
	      pf "<li><a href='%s/all.html'>%s</a></li>" path path
	    ) sl.D.Set_list.paths ;

	    List.iter ( fun path ->
	      let song = __SONG__try "find song" 
		( List.find ( fun s -> s.D.Song.path = world.D.World.root // path ) world.D.World.songs ) in
		List.iter ( fun output ->
		  if output.D.Output.filename = "all" then
		    Html.render_output world head print song output Off_line (Some sl) 
		  else 
		    ()
		) song.D.Song.outputs
	    ) sl.D.Set_list.paths ;

	    pf "
</body>
</html> " ;

	    let data = Buffer.contents buf in
	    let filename = filename ^ ".html" in
	      Zip.add_entry data zf filename
	) world.D.World.set_lists
      ) ;

      
      Zip.close_out zf ;

      pf "ok, vous pouvez télécharger <a href='/download/songs.zip'>songs.zip</a>" ;
    
      ()
)
