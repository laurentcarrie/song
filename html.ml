open Data
open Printf

let length_of_section s =
  List.fold_left ( fun acc c -> acc + c.clength ) 0 s.chords

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
}

#songname
{
text-align:center ;
font-size:5em ;
}


#measure 
{
color:red
}

div.sections
{
width:220px;
padding:10px;
border:5px solid gray;
margin:0px;
position:absolute;
left:100px;
top:150px;
}


div.lyrics
{
width:400px;
padding:10px;
border:5px solid gray;
margin:0px;
position:absolute;
left:800px;
top:150px;
}


div.structure
{
width:400px;
padding:10px;
border:5px solid gray;
margin:0px;
position:absolute;
left:800px;
top:150px;
}


" ;
    close_out fout
    
let render_html song filename = try

  print_css () ;

  let fout = open_out_bin filename in

  let print_sections () =

    let rec print_chords c offset =
      match c with
	| [] -> ()
	| chord::tl -> (
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
      let s = PMap.find sname song.sections in
	fprintf fout "<h2>%s</h2>\n" s.sname ;
	fprintf fout "<table id=\"chords\">\n" ;
	print_chords s.chords 0  ;
	fprintf fout "</table>\n" 
    in
      List.iter ( fun s -> print_section s ) song.structure
  in


  let print_structure () =
	  fprintf fout "<div class=\"structure\">\n" ;
    fprintf fout "<h2>structure</h2>\n" ;
    fprintf fout "<ol>\n" ;
    let count = ref 1 in
    List.iter ( fun sname ->
      let s = PMap.find sname song.sections in
      let l = length_of_section s in
      let count2 = !count + l in 
	fprintf fout "<li>%s  (%d : %d &rarr; %d) </li>\n" s.sname l (!count/4+1) (count2/4) ;
	count := count2  ;
    ) song.structure ;
    fprintf fout "</ol>\n" ;
fprintf fout "</div>\n"
  in

  let print_lyrics () =
      fprintf fout "<div class=\"lyrics\">\n" ;
    fprintf fout "<h2>lyrics</h2>\n" ;
    fprintf fout "<ol>\n" ;
    List.iter ( fun (sname, lyrics) ->
      fprintf fout "<li>%s<br/>\n" sname ;
      List.iter ( fun (mark,word) -> 
	    (match mark with 
	      | None -> fprintf fout "%s " word
	      | Some i -> (* if (i mod 4  = 1) && (i<>1) then fprintf fout "</br>\n" ; *) 
			     fprintf fout "<span id=\"measure\">%d %s </span>" i word
	) ;
      if word = "\n" then fprintf fout "</br>" ;
    ) lyrics ;
    fprintf fout "</li>" ;
    ) song.lyrics ;
  fprintf fout "</ol>\n" ;
fprintf fout "</div>\n" ;
  in
    


    fprintf fout "\
<html>\n\
<link rel=\"stylesheet\" type=\"text/css\" href=\"song.css\" />
<body>
<p id=\"songname\">%s</p>\n" song.name ;

			     
  fprintf fout "<div class=\"sections\">\n" ;
  print_sections () ;
  fprintf fout "</div>\n" ;
  print_structure () ;
  print_lyrics () ;
    fprintf fout "</body></html>\n" ;
  close_out fout


  with
    | e -> printf "in html, %s\n" (Printexc.to_string e) ; flush stdout ; raise e

    
