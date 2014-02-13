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
      | Not_found -> failwith("key '" ^ name ^ "' not found in map")
end

let print_css dirname = __SONG__try ("print_css " ^ dirname) (
  let fout = open_out_bin (dirname //  "song.css") in
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

div.title
{
text-align:center ;
font-size:5em ;
}

div.auteur
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
padding:10px;
border:5px solid gray;
margin:0px;
}


div.lyrics
{
padding:10px;
border:5px solid gray;
margin:0px;
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
" ;
    close_out fout
)

let render_one_html song dirname output = __SONG__try "render_html" (

    let filename = dirname // ( output.Output.filename ^ ".html") in
    let () = log "writing %s...\n" filename in
    let () = print_css dirname in

    let fout = open_out_bin filename in

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
      fprintf fout "<ol>\n" ;
      let count = ref 1 in
	List.iter ( fun s ->
	  let sname = s.Structure.section_name in
	  let s = PMap.find sname song.Song.sections in
	  let l = length_of_section s in
	  let count2 = !count + l in 
	    fprintf fout "<li>%s  (%d : %d &rarr; %d) </li>\n" s.Section.name l (!count/4+1) (count2/4) ;
	    count := count2  ;
	) song.Song.structure ;
	fprintf fout "</ol>\n" ;
    )
    in

    let print_lyrics () = __SONG__try "print_lyrics" (
      let open Data.Lyrics in
      fprintf fout "<h2>lyrics</h2>\n" ;
      fprintf fout "<ol>\n" ;
      List.iter ( fun lyrics ->
	let text = lyrics.text in
	let text = Str.global_replace (Str.regexp "\n") "<br/>" text in
	fprintf fout "<li><span class=\"lyrics-section\">%s</span><br/>\n" lyrics.name ;
	fprintf fout "%s" text ;
	fprintf fout "</li>" ;
      ) song.Song.lyrics ;
      fprintf fout "</ol>\n" ;
    )
    in
      


      fprintf fout "\
<html>\n\
<link rel=\"stylesheet\" type=\"text/css\" href=\"song.css\" />
<body>
<div class=\"title\">%s</div>\n" song.Song.title ;
      fprintf fout "\
<div class=\"auteur\">%s</div>\n" song.Song.auteur ;

      let print_item f name position =
	match position with
	  | Some position -> 
	      let open Data.Output in
		fprintf fout "<div class=\"%s\" style=\"position:absolute;%s%s%s%s\">\n"
		  name 
		  (match position.top with | None -> "" | Some i -> sprintf "top:%dcm;" i)
		  (match position.left with | None -> "" | Some i -> sprintf "left:%dcm;" i)
		  (match position.width with | None -> "" | Some i -> sprintf "width:%dcm;" i)
		  (match position.height with | None -> "" | Some i -> sprintf "height:%dcm;" i)
		;
		f () ;
		fprintf fout "</div>\n"
	  | None -> ()
      in
	print_item print_sections "sections" output.Output.grille ;
	print_item print_structure "structure" output.Output.structure ;
	print_item print_lyrics "lyrics" output.Output.lyrics ;

	fprintf fout "</body></html>\n" ;
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
