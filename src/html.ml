open Data
open Printf
open ExtList
open Util

let (//) = Filename.concat

let length_of_section s =
  List.fold_left ( fun acc c -> acc + 
    match c with
      | Section.NL -> 0
      | Section.C c -> c.Chord.length 
  ) 0 s.Section.chords

module PMap = struct 
  include PMap
  let find name m =
    try PMap.find name m with
      | Not_found -> __SONG__failwith("key '" ^ name ^ "' not found in map")
end

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

div.sections
{
padding:10px;
#border:5px solid gray;
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

let log = Fcgi.log

let render_one_html song dirname output = __SONG__try "render_html" (

    let filename = dirname // ( output.Output.filename ^ ".html") in
    let () = log "writing %s...\n" filename in

    let fout =  open_out_bin filename in

    let pf fs = ksprintf (fun s -> fprintf fout "%s\n" s) fs in

    let print_sections () = __SONG__try "print_sections" (
      
      let rec print_chords l signature offset = 
	match l with
	  | [] -> ()
	  | Section.NL::[] -> (
	      let s = if offset mod signature <> 0 then " - " else "" in
		fprintf fout "  <span class=\"note\">%s</span></td>\n</tr>\n" s ;
	    )
	  | (Section.C c)::[] -> (
	      let s = if offset mod signature <> 0 then " - " else "" in
		fprintf fout "  <span class=\"note\">%s%s</span></td>\n</tr>" (Note.html_name c.Chord.note) s
	    )
	  | Section.NL::tl -> 
	      let s = if offset mod signature <> 0 then " - " else "" in
		fprintf fout "  <span class=\"note\">%s</span></td>\n</tr>\n<tr>\n" s ;
		print_chords tl signature 0
	  | (Section.C c)::tl -> (
	      if offset mod (signature) = 0 then  fprintf fout "<td>" ;
	      (* if offset >= (signature) then __SONG__failwith "bad sequence of chord length" else () ; *)
	      fprintf fout "  <span class=\"note\">%s</span> "  (Note.html_name c.Chord.note) ;
	      let offset = offset + c.Chord.length in
		print_chords tl signature offset
	    )
      in



      let print_section name s = __SONG__try name (
	fprintf fout "<h2>%s</h2>\n" s.Section.name ;
	fprintf fout "<table id=\"chords\">\n<tr>\n" ;
	print_chords s.Section.chords s.Section.signature 0   ;
	fprintf fout "</table>\n" 
      )
      in
	List.iter ( fun (name,s) -> print_section name s ) (List.of_enum (PMap.enum song.Song.sections))
    )
    in


    let print_structure () = __SONG__try "print_structure" (
      fprintf fout "<h2>structure</h2>\n" ;
      fprintf fout "<ol class=\"structure-list\">\n" ;
      let count = ref 1 in
	List.iter ( fun s ->
	  let sname = s.Structure.section_name in
	  let s = PMap.find sname song.Song.sections in
	  let l = length_of_section s in
	  let count2 = !count + l in 
	    fprintf fout "<li class=\"structure-list\">%s  (%d : %d &rarr; %d) </li>\n" s.Section.name l (!count/4+1) (count2/4) ;
	    count := count2  ;
	) song.Song.structure ;
	fprintf fout "</ol>\n" ;
    )
    in

    let print_lyrics () = __SONG__try "print_lyrics" (
      fprintf fout "<h2>lyrics</h2>\n" ;
      fprintf fout "<ol class=\"lyrics-list\">\n" ;
      List.iter ( fun lyrics ->
	let text = lyrics.Lyrics.text in
	let text = Str.global_replace (Str.regexp "\\[\\(.*\\)\\]") (sprintf "<span class=\"lyrics-beat\">\\1</span>") text in 
	let text = Str.global_replace (Str.regexp "\n") "<br/>" text in
	fprintf fout "<li class=\"lyrics-list\"><span class=\"lyrics-section\">%s</span><br/>\n" lyrics.Lyrics.name ;
	fprintf fout "%s" text ;
	fprintf fout "</li>" ;
      ) song.Song.lyrics ;
      fprintf fout "</ol>\n" ;
    )
    in
      

      pf "\
<html>
<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />
<style>
" ;
	print_css fout dirname output.Output.width ;
	pf "
</style>
<body> " ;
	
	pf "<a href=\"index.html\">index</a>" ;

	pf " <div id=\"main-0\">

<div id=\"frame-title\">

<div class=\"title\">%s</div>\n" song.Song.title ;
      pf "\
<div class=\"auteur\">%s</div>\n" song.Song.auteur ;

pf "
<!-- frame-title -->
</div> 
" ;
    let pp =
      List.iter ( fun s ->
		    match s with
		      | Data.Output.L -> print_lyrics() ;
		      | Data.Output.G -> print_sections () ;
		      | Data.Output.S -> print_structure () ;
		) 
    in

      pf "
<div id=\"main\">
<div id=\"col_1\">
" ;
      pp output.Output.col_1 ;
pf "
</div>
<div id=\"col_2\">
" ;
pp output.Output.col_2 ;
pf "
</div>
</div>
" ;
pf "</body></html>\n" ;
	log "close %s" filename ;
	close_out fout
	  
)




let render_html song dirname = __SONG__try ("render_html " ^ dirname) (
  match song.Song.outputs with
    | [] -> log "%s" "no output defined,... you will get no html file\n"
    | outputs -> (
	List.iter ( fun output ->
	  render_one_html song dirname output 
	) outputs
      )
)
