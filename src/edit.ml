open Data
open Printf
open ExtList
open Util

let (//) = Filename.concat


let log = Fcgi.log

let render song relative_output_dir  = __SONG__try "render_html" (

  start_html_page () ;
  let plinks () = 
    pf "<a href='%s/index.html#song-%s'>index</a>" relative_output_dir song.Song.filename ;
    pf "<a href='%s/%s-all.html'>html</a>" relative_output_dir song.Song.filename ;
    pf "<a href='#lyrics'>lyrics</a>" ;
    pf "<a href='#grille'>grille</a>" ;
    pf "<a href='#structure'>structure</a>" ;
    pf "<a href='#main'>main</a>" ;
  in
    pf "</head><body>" ;
    plinks () ;
    pf "<h3><a id='lyrics'>Lyrics</a></h3>" ;
    pf "(cliquez pour éditer)" ;
    pf "<div class=\"edit\">" ;
    List.iter ( fun l ->
      let text = Str.global_replace (Str.regexp "[\n]") "<br/>"l.Lyrics.text in
	pf "<br/>%s<br/><br/>" text ;
    ) song.Song.lyrics ;
    pf "</div>" ;
    

    plinks() ;
    pf "<h3><a id='grille'>Grille</a></h3>" ;
    pf "(cliquez pour éditer)" ;
    pf "<div class=\"edit\">" ;
    PMap.iter ( fun _ section ->
      pf "<br/>%s<br/>" section.Section.name ;
      List.iter ( fun c ->
	match c with
	  | Section.NL -> pfnl "<br/>" ;
	  | Section.C c -> pfnl "%s " (Note.html_name c.Chord.note c.Chord.length) ;
      ) section.Section.chords ;
    ) song.Song.sections ;
    pf "</div>" ;
    
    end_html_page ()
)

