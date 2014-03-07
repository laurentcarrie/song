open Printf
open ExtList
open Util

module D = Data

let (//) = Filename.concat


let log = Fcgi.log

let render world onoff song   = __SONG__try "render_html" (
  let (p,end_head,end_page) = start_html_page () in
  let pf fs = ksprintf (p ~nl:true) fs in
(*   let pfnnl fs = ksprintf (p ~nl:false) fs in *)

    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-title" "titre" Edit_type.Text;
    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-auteur" "auteur" Edit_type.Text;
(*    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-filename" "filename" Edit_type.Text;*)
    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-tempo" "tempo" Edit_type.Text;
    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-lyrics" "lyrics" Edit_type.Textarea ;
    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-grille" "grille" Edit_type.Textarea ;
    Edit_type.print ~loadurl:"data.songx" p "/internal-edit.songx" song.D.Song.path "edit-lilyponds" "lilyponds" Edit_type.Textarea ;

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

    plinks() ;
    pf "<a name='lilyponds'>(cliquez pour éditer)</a>" ;
    pf "<div class=\"edit edit-lilyponds\" id='edit-lilyponds'>" ;
    Lilypond.to_html_print p onoff world song ;
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
	      let msg = Song_exn.string_of_stack e in
	      let () = Song_exn.clear_stack () in
		msg
      in
	pf "<p class='error-msg'>%s</p> " msg ;
    ) world.D.World.errors ;
    

    end_page () ;
)



let new_song world = __SONG__try "new song" (
  let (p,h,e) = start_html_page () in
  let pf fs = ksprintf p fs in
    pf "
  <style>
    label, input { display:block; }
    input.text { margin-bottom:12px; width:95; padding: .4em; }
    fieldset { padding:0; border:0; margin-top:25px; }
    h1 { font-size: 1.2em; margin: .6em 0; }
    div#users-contain { width: 550px; margin: 20px 0; }
    div#users-contain table { margin: 1em 0; border-collapse: collapse; width: 1000px; }
    div#users-contain table td, div#users-contain table th { border: 1px solid #eee; padding: .6em 10px; text-align: left; }
    .ui-dialog .ui-state-error { padding: .3em; }
    .validateTips { border: 1px solid transparent; padding: 0.3em; }
  </style>
"  ; 
    h() ;
    pf "
<div id=\"dialog-form\" title=\"Créer un nouveau morceau\">
  <p class=\"validateTips\">All form fields are required.</p>
 
  <form>
  <fieldset>
    <label for=\"path\">Chemin</label>
    <input type=\"text\" name=\"path\" id=\"path\" value='xxxxx' class=\"text ui-widget-content ui-corner-all\">
<!--
    <label for=\"email\">Email</label>
    <input type=\"text\" name=\"email\" id=\"email\" value=\"\" class=\"text ui-widget-content ui-corner-all\">
    <label for=\"password\">Password</label>
    <input type=\"password\" name=\"password\" id=\"password\" value=\"\" class=\"text ui-widget-content ui-corner-all\">
-->
  </fieldset>
  </form>
</div>
 
 
<div id=\"users-contain\" class=\"ui-widget\">
  <h1>Chemins existant :</h1>
  <table id=\"users\" class=\"ui-widget ui-widget-content\">
    <thead>
      <tr class=\"ui-widget-header \">
        <th>Path</th>
<!--
        <th>Email</th>
        <th>Password</th>
-->
      </tr>
    </thead>
    <tbody>
" ;
    let existing_paths = 
      ( List.map ( fun song -> strip_root world  song.D.Song.path ) world.D.World.songs ) @ 
	( List.map ( fun p -> strip_root world p ) world.D.World.errors )
    in
    List.iter ( fun path ->
      pf "
      <tr>
        <td>%s</td>
      </tr>
" path ) existing_paths ;
      
      pf "
    </tbody>
  </table>
</div>
<button id=\"create-user\">Créer un nouveau morceau</button> " ;
      pf "
  <script>
  $(function() {
    var path = $( \"#path\" ),
      email = $( \"#email\" ),
      password = $( \"#password\" ),
      allFields = $( [] ).add( name ).add( email ).add( password ),
      tips = $( \".validateTips\" );
 
    function updateTips( t ) {
      tips
        .text( t )
        .addClass( \"ui-state-highlight\" );
      setTimeout(function() {
        tips.removeClass( \"ui-state-highlight\", 1500 );
      }, 500 );
    }
 
    function checkLength( o, n, min, max ) {
      if ( o.val().length > max || o.val().length < min ) {
        o.addClass( \"ui-state-error\" );
        updateTips( \"Length of \" + n + \" must be between \" +
          min + \" and \" + max + \".\" );
        return false;
      } else {
        return true;
      }
    }
 
    function checkRegexp( o, regexp, n ) {
      if ( !( regexp.test( o.val() ) ) ) {
        o.addClass( \"ui-state-error\" );
        updateTips( n );
        return false;
      } else {
        return true;
      }
    }
 
    $( \"#dialog-form\" ).dialog({
      autoOpen: false,
      height: 300,
      width: 350,
      modal: true,
      buttons: {
        \"Créer\": function() {
          var bValid = true;
          allFields.removeClass( \"ui-state-error\" );
 /*
          bValid = bValid && checkLength( name, \"username\", 3, 16 );
          bValid = bValid && checkLength( email, \"email\", 6, 80 );
          bValid = bValid && checkLength( password, \"password\", 5, 16 );
 */
          if ( bValid ) {
            $( \"#users tbody\" ).append( \"<tr>\" +
              \"<td>\" + path.val() + \"</td>\" +
//+
//              \"<td>\" + email.val() + \"</td>\" +
//              \"<td>\" + password.val() + \"</td>\" +
            \"</tr>\" );
            $( this ).dialog( \"close\" );
            $.ajax({
              url:\"/internal-new.songx\",
              method:'POST',
              data:{path:path.val()},
              })
              .done(function (data) {
                  console.log('data : ' + data.success) ;
                  if ( ! data.success ) { alert(data.msg) ; }
              }) 
              .fail(function () {
                  alert('error')
                  console.log('data : ' ^ data.success) ;
              }) 
              .always(function () {
                  alert('complete')
              }) 
          }
        },
        Cancel: function() {
          $( this ).dialog( \"close\" );
        }
      },
      close: function() {
        allFields.val( \"\" ).removeClass( \"ui-state-error\" );
      }
    });
 
    $( \"#create-user\" )
      .button()
      .click(function() {
        $( \"#dialog-form\" ).dialog( \"open\" );
      });
  });
  </script> ";

      e ()
)
