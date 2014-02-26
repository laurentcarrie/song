open Data
open Printf
open ExtList
open Util

let (//) = Filename.concat


let log = Fcgi.log

let render song   = __SONG__try "render_html" (

  start_html_page () ;
  pf "</head><body>" ;
  pf "<h3>Lyrics</h3>" ;
  pf "<div class=\"edit\">" ;
  List.iter ( fun l ->
    let text = Str.global_replace (Str.regexp "[\n]") "<br/>"l.Lyrics.text in
      pf "<br/>%s<br/><br/>" text ;
  ) song.Song.lyrics ;
  pf "</div>" ;

  pf "<h3>Grille</h3>" ;
  pf "<div class=\"edit\">" ;
  PMap.iter ( fun _ section ->
    pf "<br/>%s<br/>" section.Section.name ;
    List.iter ( fun c ->
      match c with
	| Section.NL -> pf "<br/>" ;
	| Section.C c -> pf "%s\\%d" (Note.html_name c.Chord.note) c.Chord.length ;
    ) section.Section.chords ;
  ) song.Song.sections ;
  pf "</div>" ;

  end_html_page ()
)

