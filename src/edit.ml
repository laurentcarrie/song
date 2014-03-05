open Printf
open ExtList
open Util

module D = Data

let (//) = Filename.concat


let log = Fcgi.log

let render world song   = __SONG__try "render_html" (
  let (p,end_head,end_page) = start_html_page () in
  let pf fs = ksprintf (p ~nl:true) fs in
(*   let pfnnl fs = ksprintf (p ~nl:false) fs in *)

    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-title" "titre" Edit_type.Text;
    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-auteur" "auteur" Edit_type.Text;
(*    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-filename" "filename" Edit_type.Text;*)
    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-tempo" "tempo" Edit_type.Text;
    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-lyrics" "lyrics" Edit_type.Textarea ;
    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-grille" "grille" Edit_type.Textarea ;

    let filename = strip_root world song.D.Song.path in

  let plinks () = 
    pf "<a href='/index.songx#song-%s'>index</a>" filename ;
    pf "<a href='/view.songx?path=%s&output=all'>html</a>" filename ;
    pf "<a href='#lyrics'>lyrics</a>" ;
    pf "<a href='#grille'>grille</a>" ;
    pf "<a href='#structure'>structure</a>" ;
    pf "<a href='#main'>main</a>" ;
  in
    pf "</head><body>" ;
    plinks () ;
    pf "<br>titre : <div class='edit edit-title' id='edit-title'>%s</div>" song.D.Song.title ;
    pf "<br>auteur : <div class='edit edit-auteur' id='edit-auteur'>%s</div>" song.D.Song.auteur ;
    pf "<br>filename : <div class='edit edit-filename' id='edit-filename'>%s</div>" filename ;
    pf "<br>tempo : <div class='edit edit-tempo' id='edit-tempo'>%d</div>" song.D.Song.tempo ;
    plinks () ;
    pf "<a name='lyrics'>(cliquez pour éditer)</a>" ;
    pf "<div class=\"edit edit-lyrics\" id='edit-lyrics'>" ;
    Lyrics.to_html_print p song ;
    pf "</div>" ;
    

    plinks() ;
    pf "<a name='grille'>(cliquez pour éditer)</a>" ;
    pf "<div class=\"edit edit-grille\" id='edit-grille'>" ;
    Grille.to_html_print p song ;
    pf "</div>" ;
    
    end_page ()
)

let errors world    = __SONG__try "errors" (
  let (p,end_head,end_page) = start_html_page () in
  let pf fs = ksprintf (p ~nl:true) fs in
(*   let pfnnl fs = ksprintf (p ~nl:false) fs in *)

    List.iter (fun path ->
      Edit_type.print ~loadurl:"data-error.songx" p "/internal-edit-error.songx" path "edit-error" "data" Edit_type.Textarea
    ) world.D.World.errors ;

    end_head () ;

    List.iter ( fun path ->
      pf "<h2>%s</h2>" (strip_root world path) ;
      pf "<div class=\"edit edit-error\" id='edit-error'>" ;
      pf "%s" (Std.input_file path) ;
      pf "</div>" ;
      let msg = try
	  let (_:D.Song.t) = Rw.from_file path in ""
	with
	  | e ->
	      let msg = Song_exn.string_of_stack () in
	      let () = Song_exn.clear_stack () in
		msg
      in
	pf "<p class='error-msg'>%s</p> " msg ;
    ) world.D.World.errors ;
    

    end_page () ;
)

