open Printf
open Util

module D = Data

let (//) = Filename.concat

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
	) song.D.Song.outputs
      ) world.D.World.songs ;
      
      Zip.close_out zf ;

      pf "ok, vous pouvez télécharger <a href='/download/songs.zip'>songs.zip</a>" ;
    
      ()
)
