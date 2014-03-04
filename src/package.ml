open Data
open Printf
open Util

let (//) = Filename.concat

let make_zip world print =
  
  let pf fs = ksprintf print fs in


    let zf = Zip.open_out (world.World.output_root // "songs.zip") in

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
	  let () = Index.write_index print world in
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
	    let () = Html.render_output world print song output in
	    let data = Buffer.contents buf in
	    let filename = song.Song.filename // (sprintf "%s.html" output.Output.filename) in
	      Zip.add_entry data zf filename
	) song.Song.outputs
      ) world.World.songs ;
      
      Zip.close_out zf ;

      pf "ok, vous pouvez télécharger ";
    
      ()
