open Printf
open Util
open ExtList

module D = Data

let (//) = Filename.concat

let log = Fcgi.log

let make_zip world print = __SONG__try "make_zip" (
  
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
	let () = Css.print_css print in
	  Zip.add_entry (Buffer.contents buf) zf "song.css"
      ) ;

      List.iter ( fun song ->


	List.iter ( fun output ->
	  let buf = Buffer.create 1024 in
	  let print = Buffer.add_string buf in
	  let pf fs = ksprintf print fs in
	    pf "
<html>
<head>
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
<link rel=\"stylesheet\" href=\"../song.css\"/>
" ;
	    let () = Html.render_output world print song output Off_line in
	    let data = Buffer.contents buf in
	    let filename = strip_root world song.D.Song.path in
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
      
      Zip.close_out zf ;

      pf "ok, vous pouvez télécharger <a href='/download/songs.zip'>songs.zip</a>" ;
    
      ()
)
