open Data
open Printf
open ExtList

let length_of_section s =
  List.fold_left ( fun acc c -> acc + c.Chord.length ) 0 s.Section.chords

module PMap = struct 
  include PMap
  let find name m =
    try PMap.find name m with
      | Not_found -> failwith("key '" ^ name ^ "' not found in map")
end

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

td.half-chords
{
width:25px ;
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
width:320px;
padding:10px;
border:5px solid gray;
margin:0px;
position:absolute;
left:2cm;
top:4cm;
}


div.lyrics
{
width:400px;
padding:10px;
border:5px solid gray;
margin:0px;
position:absolute;
left:20cm;
top:4cm;
}


div.structure
{
width:6cm;
padding:10px;
border:5px solid gray;
margin:0px;
position:absolute;
left:10cm;
top:4cm ;
}

span.lyrics-section{
background-color:#eeaaee ;
}

span.note {
background-color:red
word-spacing:1px
}
" ;
    close_out fout
      
let render_html song filename = __SONG__try "render_html" (

    let () = printf "render_html\n" ; flush stdout in

    let () = print_css () in

    let fout = open_out_bin filename in

    let print_sections () =
      
      let rec print_chords c offset tailles_lignes = 
	match tailles_lignes with
	  | [] -> __SONG__failwith "internal error"
	  | 0::[] -> (
	      fprintf fout "%s</td></tr>" ( if offset mod 4 = 0 then "" else "|") ;
	      if c <> [] then __SONG__failwith ("no more line, but remaining chords")
	    )
	  | 0::tl -> fprintf fout "%s</td></tr><tr>"  ( if offset mod 4 = 0 then "" else "|") ; print_chords c 0 tl
	  | hd_tailles_lignes::tl_tailles_lignes -> (
	      match c with
		| [] -> __SONG__failwith ("no more chords")
		| chord::tl -> (
		    let open Data.Chord in
		    let cell = offset / 4 in 
		      if offset mod 4  = 0 then fprintf fout "<td>" ;
		      fprintf fout "<span class=\"note\">%c%s%s%s%s</span> \n" 
			(Char.uppercase chord.name) 
			(match chord.alteration with | Flat -> "<sup>&#x266d</sup>" | Sharp -> "<sup>&#9839</sup>" | Rien -> "")
			(if chord.minor then "m" else "")
			(if chord.mi7 then "7" else "")
			(if chord.ma7 then "7M" else "")
		      ;  
		      let new_offset = offset + chord.length in
			(* let new_row = new_offset / 16 in *)
			(* if new_row <> row then failwith "row" ; *)
		      let new_cell = new_offset / 4 in 
			if new_cell <> cell &&  new_offset mod 4 <> 0 then
			  (* failwith "bad end cell" ; *) () else () ;
			if new_offset mod 4  = 0 then (
			  fprintf fout "</td>" ;
			  (* if new_offset mod 16 = 0 then fprintf fout "</tr>\n" ; *)
			) ;
			print_chords tl new_offset (hd_tailles_lignes-chord.length::tl_tailles_lignes)
		  )
	    )
      in



      let print_section name s = __SONG__try name (
	fprintf fout "<h2>%s</h2>\n" s.Section.name ;
	fprintf fout "<table id=\"chords\">\n" ;
	let taille_lignes = match s.Section.mesures_par_ligne with
	  | Some l -> printf "HHHHHHHHHHHHHhhhas taille\n"  ; l
	  | None -> let n = (List.length s.Section.chords) / 16 + 1 in Array.to_list (Array.make n 16) 
	in
	print_chords s.Section.chords 0 (taille_lignes)  ;
	fprintf fout "</table>\n" 
      )
      in
	List.iter ( fun (name,s) -> print_section name s ) (List.of_enum (PMap.enum song.Song.sections))
    in


    let print_structure () =
      fprintf fout "<div class=\"structure\">\n" ;
      fprintf fout "<h2>structure</h2>\n" ;
      fprintf fout "<ol>\n" ;
      let count = ref 1 in
	List.iter ( fun sname ->
	  let s = PMap.find sname song.Song.sections in
	  let l = length_of_section s in
	  let count2 = !count + l in 
	    fprintf fout "<li>%s  (%d : %d &rarr; %d) </li>\n" s.Section.name l (!count/4+1) (count2/4) ;
	    count := count2  ;
	) song.Song.structure ;
	fprintf fout "</ol>\n" ;
	fprintf fout "</div>\n"
    in

    let print_lyrics () =
      fprintf fout "<div class=\"lyrics\">\n" ;
      fprintf fout "<h2>lyrics</h2>\n" ;
      fprintf fout "<ol>\n" ;
      List.iter ( fun (sname, lyrics) ->
	fprintf fout "<li><span class=\"lyrics-section\">%s</span><br/>\n" sname ;
	List.iter ( fun (mark,word) -> 
	  (match mark with 
	    | None -> fprintf fout "%s " word
	    | Some i -> (* if (i mod 4  = 1) && (i<>1) then fprintf fout "</br>\n" ; *) 
		fprintf fout "<span id=\"measure\">%d %s </span>" i word
	  ) ;
	  if word = "\n" then fprintf fout "</br>" ;
	) lyrics ;
	fprintf fout "</li>" ;
      ) song.Song.lyrics ;
      fprintf fout "</ol>\n" ;
      fprintf fout "</div>\n" ;
    in
      


      fprintf fout "\
<html>\n\
<link rel=\"stylesheet\" type=\"text/css\" href=\"song.css\" />
<body>
<p id=\"songname\">%s</p>\n" song.Song.name ;

      
      fprintf fout "<div class=\"sections\">\n" ;
      print_sections () ;
      fprintf fout "</div>\n" ;
      print_structure () ;
      print_lyrics () ;
      fprintf fout "</body></html>\n" ;
      close_out fout

)

	
