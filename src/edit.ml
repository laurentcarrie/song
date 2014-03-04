open Data
open Printf
open ExtList
open Util

let (//) = Filename.concat


let log = Fcgi.log

let render world song   = __SONG__try "render_html" (
  let (p,end_head,end_page) = start_html_page () in
  let pf fs = ksprintf (p ~nl:true) fs in
(*   let pfnnl fs = ksprintf (p ~nl:false) fs in *)

    Edit_type.print p "/internal-edit.songx" song.Song.path "edit-title" "titre" Edit_type.Text;
    Edit_type.print p "/internal-edit.songx" song.Song.path "edit-auteur" "auteur" Edit_type.Text;
    Edit_type.print p "/internal-edit.songx" song.Song.path "edit-filename" "filename" Edit_type.Text;
    Edit_type.print p "/internal-edit.songx" song.Song.path "edit-tempo" "tempo" Edit_type.Text;
    Edit_type.print p "/internal-edit.songx" song.Song.path "edit-lyrics" "lyrics" Edit_type.Textarea ;
    Edit_type.print p "/internal-edit.songx" song.Song.path "edit-grille" "grille" Edit_type.Textarea ;

  let plinks () = 
    pf "<a href='/index.songx#song-%s'>index</a>" song.Song.filename ;
    pf "<a href='/view.songx?path=%s&output=all'>html</a>" song.Song.filename ;
    pf "<a href='#lyrics'>lyrics</a>" ;
    pf "<a href='#grille'>grille</a>" ;
    pf "<a href='#structure'>structure</a>" ;
    pf "<a href='#main'>main</a>" ;
  in
    pf "</head><body>" ;
    plinks () ;
    pf "<br>titre : <div class='edit edit-title' id='edit-title'>%s</div>" song.Song.title ;
    pf "<br>auteur : <div class='edit edit-auteur' id='edit-auteur'>%s</div>" song.Song.auteur ;
    pf "<br>filename : <div class='edit edit-filename' id='edit-filename'>%s</div>" song.Song.filename ;
    pf "<br>tempo : <div class='edit edit-tempo' id='edit-tempo'>%d</div>" song.Song.tempo ;
    plinks () ;
    pf "<a name='lyrics'>(cliquez pour éditer)</a>" ;
    pf "<div class=\"edit edit-lyrics\" id='edit-lyrics'>" ;
    Lyrics_of_file.to_html_print p song ;
    pf "</div>" ;
    

    plinks() ;
    pf "<a name='grille'>(cliquez pour éditer)</a>" ;
    pf "<div class=\"edit edit-grille\" id='edit-grille'>" ;
    
    Grille_of_file.to_html_print p song ;
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

