open Printf
open ExtList
open Util
module D = Data

let (//) = Filename.concat

let length_of_section s =
  List.fold_left ( fun acc c -> acc + 
    match c with
      | D.Section.NL -> 0
      | D.Section.C c -> c.D.Chord.length 
  ) 0 s.D.Section.chords

module PMap = struct 
  include PMap
  let find name m =
    try PMap.find name m with
      | Not_found -> __SONG__failwith("key '" ^ name ^ "' not found in map")
end

(*
let print_css fout dirname width = __SONG__try ("print_css " ^ dirname) (
(*  let fout = open_out_bin (dirname //  "song.css") in *)
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
width:2cm ;
}

td.half-chords
{
width:1cm ;
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

div.title
{
padding:10px;
text-align:center ;
font-size:2em ;
}

div.auteur
{
padding:10px;
text-align:center ;
font-size:1.1em ;
}


#measure 
{
color:red
}

div#error-msg {
border:5px solid gray;
background-color:#FFEEEE ; 
}


div.sections
{
padding:10px;
margin:0px;
}

div#main-0 
{
padding:10px;
width:%dcm ;
/* border:1px solid #98bf21; */
}

div#frame-title
{
padding:10px;
 background-color:#EEFFEE ; 
border:1px solid #98bf21;
}

div#main
{
/* border:5px solid pink ;*/
}

div#col_1
{
padding:10px;
float:left ;
/* border:5px solid #98bf21; */
/*background-color:#FFEEEE ; */
}

div#col_2
{
padding:10px;
float:right ;
/* top:2cm ; */
/* border:5px solid gray ;*/
/* background-color:#EEFFEE ; */

}

.lyrics-list {
padding : 10px ;
}

div.lyrics
{
padding:10px;
/* border:5px solid gray; */
margin:0px;
}

.lyrics-beat {
/* color:#ff0000 ; */
text-decoration:underline ;
font-weight:bold ;
}

div.structure
{
padding:10px;
border:5px solid gray;
margin:0px;
}

span.lyrics-section{
background-color:#eeaaee ;
}

span.note {
background-color:red
word-spacing:1px
}

" width 
)
*)

let log = Fcgi.log


let render_output world print song output onoff = __SONG__try "render_html" (
    
  let pf fs = ksprintf print fs in
    
  let print_structure () = __SONG__try "print_structure" (
    pf "<h2>structure</h2>\n" ;
    pf "<ol class=\"structure-list\">\n" ;
    let count = ref 1 in
      List.iter ( fun s ->
	let sname = s.D.Structure.section_name in
	let s = try List.find ( fun current -> current.D.Section.name = sname) song.D.Song.sections with
	  | Not_found -> __SONG__failwith ("pas de structure nommee " ^ sname )in
	let l = length_of_section s in
	let count2 = !count + l in 
	  (* pf "<li class=\"structure-list\">%s  (%d : %d &rarr; %d) </li>\n" s.D.Section.name l (!count/4+1) (count2/4) ; *)
	  pf "<li class=\"structure-list\">%s</li>\n" s.D.Section.name ; 
	  count := count2  ;
      ) song.D.Song.structure ;
      pf "</ol>\n" ;
  )
  in

  let filename = strip_root world song.D.Song.path in
      
  let () = match onoff with
      | On_line -> (
	  pf "<a href=\"/index.songx#song-%s\">index</a>" filename ;
	  pf "<a href=\"/edit.songx?path=%s\">edit</a>" song.D.Song.path ;
	  List.iter ( fun o ->
	    pf "<a href=\"%s.html\">(%s)</a>" o.D.Output.filename o.D.Output.filename
	  ) song.D.Song.outputs ;
	  pf "<a href=\"%s.midi\"><span class=\"index-title\">(midi)</span></a>" filename ;
	  pf "<a href=\"%s.wav\"><span class=\"index-title\">(wav)</span></a>" filename ;
	  pf "<a href=\"%s.mp3\"><span class=\"index-title\">(mp3)</span></a>" filename ;
	  pf "<a href=\"%s.pdf\"><span class=\"index-title\">(pdf)</span></a>" filename ;
	  pf "<br/>" ;
	)
      | Off_line -> (
	  pf "<a href=\"../index.html#song-%s\">index</a>" filename ;
	  pf "<br/>" ;
	)
  in
    
    pf " <div id=\"main-0\">

<div id=\"frame-title\">

<div class=\"title\">%s</div>\n" song.D.Song.title ;
      pf "\
<div class=\"auteur\">%s</div>\n" song.D.Song.auteur ;

pf "
<!-- frame-title -->
</div> 
" ;
    let pp =
      List.iter ( fun s ->
		    match s with
		      | D.Output.L -> Lyrics.to_html_print print song ;
		      | D.Output.G -> Grille.to_html_print print song ;
		      | D.Output.S -> print_structure () ;
		) 
    in

      pf "
<div id=\"main\">
<div id=\"col_1\">
" ;
      pp output.D.Output.col_1 ;
pf "
</div>
<div id=\"col_2\">
" ;
pp output.D.Output.col_2 ;
pf "
</div>
</div>
" ;
	  
)



(*
let render_html world print song  = __SONG__try ("render_html ") (
  match song.D.Song.outputs with
    | [] -> log "%s" "no output defined,... you will get no html file\n"
    | outputs -> (
	List.iter ( fun output ->
	  render_one_html world song output
	) outputs
      )
)
*)
