open Data
open Printf
open ExtList
open Util

let (//) = Filename.concat


let log = Fcgi.log

let render song relative_output_dir : unit  = __SONG__try "render_html" (
  let (p,end_head,end_page) = start_html_page () in
  let pf fs = ksprintf (p ~nl:true) fs in
  let pfnnl fs = ksprintf (p ~nl:false) fs in


    print_edit p "/internal-edit.songx" song.Song.path "edit-lyrics" "lyrics" ;
    print_edit p "/internal-edit.songx" song.Song.path "edit-grille" "grille" ;

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
    pf "<div class=\"hide-me edit-hidden\" id='edit-lyrics-hidden'>" ;
    List.iter ( fun l ->
      pf "%s" l.Lyrics.text ;
    ) song.Song.lyrics ;
    pf "</div>" ;
    pf "<div class=\"edit edit-lyrics\" id='edit-lyrics'>" ;
    List.iter ( fun l ->
      let text = Str.global_replace (Str.regexp "[\n]") "<br/>"l.Lyrics.text in
	pf "<br/>%s<br/><br/>" text ;
    ) song.Song.lyrics ;
    pf "</div>" ;
    

    plinks() ;
    pf "<h3><a id='grille'>Grille</a></h3>" ;
    pf "(cliquez pour éditer)" ;
    pf "<div class=\"hide-me edit-hidden\" id='edit-grille-hidden'>" ;
    PMap.iter ( fun _ section -> 
      pf "%s" section.Section.name ;
      List.iter ( fun c ->
	match c with
	  | Section.NL -> pf "" ;
	  | Section.C c -> pfnnl  "%s " (Note.text_name c.Chord.note c.Chord.length) ;
      ) section.Section.chords ;
      pf "" ;
    ) song.Song.sections ;
    pf "</div>" ;
    pf "<div class=\"edit edit-grille\" id='edit-grille'>" ;
    
    Html.render_sections song p ;
    (*
    PMap.iter ( fun _ section ->
      pf "<br/>%s<br/>" section.Section.name ;
      List.iter ( fun c ->
	match c with
	  | Section.NL -> pf "<br/>" ;
	  | Section.C c -> pf  "%s " (Note.html_name c.Chord.note c.Chord.length) ;
      ) section.Section.chords ;
    ) song.Song.sections ;
    *)
    pf "</div>" ;
    
    end_page ()
)

