open Data
open Printf

let print_css () =
  let fout = open_out_bin "song.css" in
    fprintf fout "
#chords
{
font-family:\"Trebuchet MS\", Arial, Helvetica, sans-serif;
#width:100%%;
border-collapse:collapse;
}
#chords td, #chords th 
{
font-size:1em;
border:1px solid #98bf21;
padding:2px 2px 2px 2px;
text-align:center;
height:50px ;
width:50px ;
}
#chords th 
{
font-size:1.1em;
text-align:left;
padding-top:5px;
padding-bottom:4px;
background-color:#A7C942;
color:#ffffff;
}
#chords tr.alt td 
{
color:#000000;
background-color:#EAF2D3;
}table, th, td

" ;
    close_out fout
    
let render_html song filename =

  print_css () ;

  let fout = open_out_bin filename in

  let print_sections () =

    let rec print_chords c offset =
      match c with
	| [] -> ()
	| chord::tl -> (
	    (* let row = offset / 16 in *)
	    let cell = offset / 4 in 
	      if offset mod 16 = 0 then fprintf fout "<tr>\n" ;
	      if offset mod 4  = 0 then fprintf fout "<td>" ;
	      (* fprintf fout "%c (%d)\n" (Char.uppercase chord.cname) chord.clength ;  *)
	      fprintf fout "%c%s%s%s \n" 
		(Char.uppercase chord.cname) 
		(if chord.minor then "m" else "")
		(if chord.mi7 then "7" else "")
		(if chord.ma7 then "7M" else "")
	      ;  
	      let new_offset = offset + chord.clength in
		printf "offset : %d -> %d\n" offset new_offset ;
		(* let new_row = new_offset / 16 in *)
		(* if new_row <> row then failwith "row" ; *)
		let new_cell = new_offset / 4 in 
		  if new_cell <> cell &&  new_offset mod 4 <> 0 then
		    failwith "bad end cell" ;
		  if new_offset mod 4  = 0 then (
		    fprintf fout "</td>" ;
		    if new_offset mod 16 = 0 then fprintf fout "</tr>\n" ;
		  ) ;
		  print_chords tl new_offset
	  )
    in



    let print_section sname =
      printf "%s\n" sname ;
      let s = PMap.find sname song.sections in
	fprintf fout "<h2>%s</h2>\n" s.sname ;
	fprintf fout "<table id=\"chords\">\n" ;
	print_chords s.chords 0  ;
	fprintf fout "</table>\n" 
    in
      List.iter ( fun s -> print_section s ) song.structure
  in

    fprintf fout "\
<html>\n\
<link rel=\"stylesheet\" type=\"text/css\" href=\"song.css\" />
<h1>%s</h1>\n" song.name ;
    print_sections () ;
    fprintf fout "</html>\n" ;
    close_out fout




    
