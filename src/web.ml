open Printf
open Util
open ExtString
open ExtList

module D = Data

let (//) = Filename.concat 

type args = ( string * string ) list

let log = Fcgi.log




let log = Fcgi.log


let plog = Fcgi.c_fcgi_log_string

let main_loop  () = __SONG__try "main loop" (
    log "test fcgi_log" ;
    Array.iter ( fun a ->
      log "arg : %s" a
    ) Sys.argv ;
    
  (
    Array.iter ( fun s ->
      log "env :%s" s
    ) (Unix.environment())
  ) ;
  
(* let root = (D.world()).D.World.root in *)
  let output_root = (D.world()).D.World.output_root in
  let doc_root = (D.world()).D.World.doc_root in
  log "output_root = %s" output_root ; 
  log "doc_root = %s" doc_root ;
(*
  log "relative_output_dir = %s" relative_output_dir ; 
  log "root_path = %s" root ;
*)
  
  let process_request () = 
    try
      let script_name = try 
	  let script_name = __SONG__try "script name" (Unix.getenv "SCRIPT_NAME") in
	    log "script : %s" script_name ;
	    (* Str.global_replace (Str.regexp ".songx") "" script_name*)
	    script_name
	with
	  | Not_found -> "noscript"
      in
	log "script name : %s" script_name ;
	match script_name with 
	  | "/update.songx" -> (
	      log "== BEGIN script : %s"  script_name ;
	      (* let song_paths = Generate.find_all_songs ~plog:Fcgi.c_fcgi_log_string ~root in *)
	      let song_paths = [] in
	      let (print,end_head,end_page) = start_html_page () in
	      let pf fs = ksprintf print fs in
		pf "<script>

" ;

		pf "to_go = %d ; " (List.length song_paths) ;
		pf "

var nb_errors = 0 ;
var nb_ok_unchanged = 0 ;
var nb_ok_generated = 0 ;
var nb_ok_failed = 0 ;
var songs = new Array() ;
" ;

		pf "
function update_index () {
console.log('body of update_index() ' + to_go) ;
$.ajax({
    type: 'POST',
    url:  'update_index.songx',
    data: {'songs':songs},
    success: function(data) { console.log('index generated ' + data.nb_songs + ' songs') ; },
    error: function(xhr,ajaxOptions,thrownError) {console.log('error '+ xhr.responseText) ; }
}) ;
} ;
" ;
		pf "
function add_result(data) {
        $('#song-todo-'+data.index).attr('class','hide-me') ;
        if (data.success == true) {
          if (data.generated==true) {
            nb_ok_generated = nb_ok_generated + 1 ;
            $('#title-ok_generated').text(nb_ok_generated+' chansons mises à jour') ;
            $('#title-ok_generated').attr('class','div-title') ;
            $('#div-ok_generated').attr('class','div-count') ;
            $('#song-'+data.index).attr('class','ok_generated') ;
            $('#song-updated').append('<li class=\"ok_generated\">' + data.path + '</li>') ;
          } else {
            nb_ok_unchanged = nb_ok_unchanged + 1 ;
            $('#title-ok_unchanged').text(nb_ok_unchanged+' chansons non changées') ;
            $('#title-ok_unchanged').attr('class','div-title') ;
            $('#song-'+data.index).attr('class','ok_unchanged') ;
            $('#div-ok_unchanged').attr('class','div-count') ;
            $('#song-unchanged').append('<li class=\"ok_unchanged\">' + data.path + '</li>') ;
          }
          songs[data.index] = data.song ;
        } else {
          $('#song-'+data.index).attr('class','ok_failed') ;
          nb_errors = nb_errors + 1 ;
          $('#nb-errors').text(nb_errors + ' error(s)') ;
            nb_ok_failed = nb_ok_failed + 1 ;
            $('#title-ok_failed').text(nb_ok_failed+' chansons comportant une erreur') ;
            $('#title-ok_failed').attr('class','div-title') ;
            $('#song-'+data.index).attr('class','ok_failed') ;
            $('#div-ok_failed').attr('class','div-count') ;
          $('#song-error').append('<li class=\"ok-failed\">' + data.path + '</li>') ;
          $('#errors').append('<li class=\"error\"><pre>'+data.msg+'</pre></li>') ;
        }
        to_go = to_go-1 ;
        if (to_go>0) {   
          $('#title-a-traiter').text(to_go + ' restant a faire') ;
          // $('#status').text(to_go + ' restant a faire') ; 
        }
        else {  
           update_index() ;
           $('#div-a-traiter').attr('class','hide-me') ;
           $('#status').text('fini !') ; 
        } 
} ;


function show_end() {

}  ;

function manage(module,index) {
console.log(module) ;
$.ajax({
    type: 'POST',
    url:  'generate-song.songx',
    data: {path:module,index:index},
    success: function(data) { console.log('success '+ data.path + '...' + data.index) ; add_result(data) ; },
    error: function(xhr,ajaxOptions,thrownError) {console.log('error '+ xhr.responseText) ; }
}) ;
} ;

$(document).ready(function () { 
"; 
		let (song_paths,_) = List.fold_left ( fun (acc,count) s -> ((s,count)::acc),count+1) ([],0) song_paths in
		let stripped_song_paths = List.map ( fun (s,i) -> Str.global_replace (Str.regexp (Str.quote output_root)) "" s,i) song_paths in
		let () = List.iter ( fun (s,i) -> pf "manage('%s',%d) ;" s i ) song_paths in
		pf "show_end() ;";
		pf "
}) ;
</script> 
"; 
		
		pf "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />" ;
		pf "<style>
div.error-msg {
background-color:#FFEEEE ; 
}
</style>
" ; 
		
	    end_head () ;

	    pf "
<ul id='songs'>" ;
		(* List.iter ( fun (s,i) -> pf "<li id='song-%d' class='not_done'> %s </li>" i s ) stripped_song_paths ; *)
		
		pf "</ul>
<div id='show_end'></div> 
</div> " ;
		pf "
<div id='div-a-traiter'>
  <p id='title-a-traiter' class='div-title'></p>" ;
		pf "<ul id='song-todo'>";
		List.iter ( fun (path,i) ->
		  pf "<li id='song-todo-%d' class='todo'>%s</li>" i path 
		) stripped_song_paths ;
		pf "</ul>";
		pf "
</div> "; 

		pf "
<div id='div-ok_generated' class='hide-mex'>
  <p id='title-ok_generated'></p>
  <ul id='song-updated'></ul>
</div>"; 
		
		pf "
<div id='div-ok_unchanged' class='hide-mex'>
  <p id='title-ok_unchanged'></p>
  <ul id='song-unchanged'></ul>
</div> ";
		
		pf "
<div id='div-ok_failed' class='div-mex'>
  <p id='title-ok_failed'></p>
  <ul id='song-error'></ul>
</div>" ;
		
		pf "<div id='status'>%d restant a faire</div>" (List.length song_paths) ;
		pf "<div id='index'><a href='/index.songx'>index</a></div>"  ;
		pf "<p id='nb-errors'>0 errors</p>" ;
		pf "<ul id='errors'/>" ;
		pf "<ul id='message'/>" ;
		pf "<div id='bin'/>" ;

		
(*
		let songs = 
		  (*manage_all_songs   ~root ~output_dir ~doc_root ~relative_output_dir  ~root_path  [] root in
		  *)
		  [] in
		  (* Index.write_index ~songs ~root_path ~output_dir ~relative_output_dir ; *)
*)
		log "== END script : %s"  script_name ;
		end_page () ;
	    ) 
	      
	  | "/css.songx" -> (
	      try 
		let (p,e) = start_page 200 "text/css"  in
		  Css.print_css p ;
		  e ()
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j
		  )
	    )
	  | "/reload.songx" -> (
	      try
		let world = D.world () in
		let (songs,errors) = Rw.all_songs_from_root world.D.World.root in
		let world = D.update_world_songs world songs in
		let _ = D.update_world_errors world errors in
		let (p,h,e) = start_html_page () in
		let pf fs = ksprintf p fs in
		  h () ;
		  pf "ok, %d songs reloaded<br>" (List.length songs) ;
		  pf "ok, %d errors found<br>" (List.length errors) ;
		  pf "<a href='/index.songx'>index</a>";
		  e ()
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j
		  )
	
	    )
	  | "/index.songx" -> (
	      try
		let (p,h,e) = start_html_page () in
		  h () ;
		  Index.write_index p (D.world()) On_line ;
		  e ()
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j
		  )
	
	    )
	  | "/errors.songx" -> (
	      try
		let world = D.world () in
		  Edit.errors world 
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j
		  )
	    )
	  | "/download.songx" -> (
	      try
		let world = D.world () in
		let songs = List.map ( fun song ->
		  Lilypond.generate_png world song
		) world.D.World.songs in
		let world = D.update_world_songs world songs in
		let (p,h,e) = start_html_page () in
		  Package.make_zip world p ;
		  e ()
	      with
		| e -> (
		    error_page ()
		  )
	    )
	  | "/view.songx" -> (
	      try
		let world = D.world () in
		let params = Fcgi.parse_query_string () in
		let path = __SONG__try "path" ( List.assoc "path" params) in
		let path = normalize_path (world.D.World.root // path) in
		let song = try List.find ( fun s -> s.D.Song.path = path ) world.D.World.songs
		  with | Not_found -> __SONG__failwith ("pas de chanson trouvée pour : "^ path) in
		let song = Lilypond.generate_png world song in
		let world = D.update_song world song in
		let output = __SONG__try "output" ( List.assoc "output" params) in
		let output = try List.find ( fun o -> o.D.Output.filename = output ) song.D.Song.outputs 
		  with | Not_found -> __SONG__failwith ("pas de sortie trouvée pour : " ^ output) in
		let (print,h,e) = start_html_page () in
		  h () ;
		  Html.render_output world print song output On_line ;
		  e ()
	      with
		| e -> (
		    error_page ()
		  )
	    )
	  | "/env.songx" -> (
	      let env = Unix.environment () in
	      let buf = Buffer.create 1024 in
	      let pf fs = ksprintf (fun s -> Buffer.add_string buf s ; Buffer.add_string buf "\n") fs in
		Array.iter ( fun s -> pf "%s" s ) env ;
		text_page (Buffer.contents buf)
	    )
	  | "/edit.songx" 
	  | "/edit_song.songx" -> ( 
	      try
		let world = D.world () in
		let params = Fcgi.parse_query_string () in
		let path = __SONG__try "path" ( List.assoc "path" params) in
		let song = List.find ( fun s -> s.D.Song.path = path ) world.D.World.songs in
		let song = Lilypond.generate_png world song in
		let world = D.update_song world song in
		  Edit.render world On_line song 
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j 
		  )
	    )
	  | "/new.songx" -> ( 
	      try
		let world = D.world () in
		  Edit.new_song world 
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j 
		  )
	    )
	  | "/data.songx" -> (
	      try
		let world = D.world () in
		let params = Fcgi.parse_query_string () in
		let path  = __SONG__try "path"  (List.assoc "path" params) in
		let field = __SONG__try "field"  (List.assoc "field" params) in
		let song = List.find ( fun s -> s.D.Song.path = path ) world.D.World.songs in
		let text = (match field with
		    | "grille" ->  Grille.to_string song
		    | "lyrics" -> Lyrics.to_string song
		    | "titre" -> song.D.Song.title
		    | "auteur" -> song.D.Song.auteur
		    | "lilyponds" -> Lilypond.to_string song
(*		    | "filename" -> song.D.Song.filename*)
		    | "tempo" -> string_of_int song.D.Song.tempo
		    | s -> __SONG__failwith ("champ inconnu : " ^ field )
		) in
		  text_page text
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		      (*
			let j = Json_type.Build.objekt [
			"success",Json_type.Build.bool false ;
			"msg",Json_type.Build.string msg ;
			] in
		      *)
		      text_page msg
		  )
	    )
	  | "/data-error.songx" -> (
	      try
		let params = Fcgi.parse_query_string () in
		let path  = __SONG__try "path"  (List.assoc "path" params) in
		  text_page (Std.input_file path)
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j 
		  )
	    )
	  | "/internal-edit.songx" -> ( 
	      try
		let world = D.world () in 
		let params = Fcgi.get_post_params() in
		let () = log "NBPARAMS : %d"( List.length params) in
		let () = List.iter ( fun (key,_) -> log "KEY : %s" key ) params in
		let textval = __SONG__try "get value" (List.assoc "value" params) in
		let () = log "TEXTVAL : %s"  textval in
		let path = __SONG__try "get path" (List.assoc "path" params) in
		let field = __SONG__try "field" (List.assoc "field" params) in
		(* let (song:D.Song.t) = __SONG__try "read" ( Generate.song_of_path path) in  *)
		let song = List.find ( fun s -> s.D.Song.path = path ) world.D.World.songs in
		let (song,html_textval) =  match field with
		  | "lyrics" -> let song = Lyrics.update_data song textval in song,Lyrics.to_html song
		  | "grille" -> let song = Grille.update_data song textval in song,Grille.to_html song
		  | "tempo" -> let song = { song with D.Song.tempo = int_of_string textval } in song,textval
		  | "titre" -> let song = { song with D.Song.title = textval } in song,textval
		  | "auteur" -> let song = { song with D.Song.auteur = textval } in song,textval
		  | "lilyponds" -> (
		      let song = Lilypond.update_data song textval in 
		      let song = Lilypond.generate_png world song in
			song,Lilypond.to_html world On_line song 
		    )
		  | s -> __SONG__failwith ("champ inconnu : " ^ field )
		in
		let () = Rw.write_song song in
		let _ = D.update_song world song in
		(* let ((_:bool)) = Generate.generate_from_song ~world ~plog ~print:(fun _->()) ~path song in *)
		  text_page html_textval
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j ;
		  )
	    )
	  | "/json-of.songx" -> ( 
	      try
		let world = D.world () in 
		let params = Fcgi.parse_query_string () in
		let path = __SONG__try "get % path" (List.assoc "path" params) in
		let path = normalize_path (world.D.World.root // path) in
		  log "json-of , path = '%s'" path ;
		let song = __SONG__try "find song" (List.find ( fun s -> s.D.Song.path = path ) world.D.World.songs) in
		let j = Data_j.Song.to_json song in
		let s = Json_io.string_of_json j in
		  log "j = %s ; size %d" s (String.length s) ;
		  if false then
		    text_page "hello world" 
		  else
		    json_page j
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j ;
		  )
	    )
	  | "/internal-edit-error.songx" -> ( 
	      try
		let world = D.world () in 
		let params = Fcgi.get_post_params() in
		let textval = __SONG__try "get value" (List.assoc "value" params) in
		let path = __SONG__try "get path" (List.assoc "path" params) in
		let () = __SONG__try "write file" (Std.output_file ~filename:path ~text:textval) in
		let (_:D.Song.t) = __SONG__try "read file" (Rw.from_file path) in
		let (songs,errors) = __SONG__try "rw" (Rw.all_songs_from_root world.D.World.root) in
		let world = D.update_world_songs world songs in
		let world = D.update_world_errors world errors in
		  text_page (sprintf "ok, %s is now valid" (strip_root world path)) 
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j ;
		  )
	    )
	  | "/internal-new.songx" -> ( 
	      try
		let world = D.world () in 
		let params = Fcgi.get_post_params() in
		let path = __SONG__try "get path" (List.assoc "path" params) in
		let whole_path = normalize_path (world.D.World.root // (path ^ ".song")) in
		let () = try
		    let (_:D.Song.t) = List.find ( fun s -> 
		      log "compare\n'%s' and\n'%s'" s.D.Song.path whole_path ;
		      s.D.Song.path = whole_path ) world.D.World.songs in
		      __SONG__failwith ("il y a dejà un morceau pour ce chemin : " ^ path )
		  with
		    | Not_found -> ()
		in
		let song = {
		  D.Song.title = "???????????????????????" ;
		  auteur = "???????????????????????" ;
		  format = None ;
		  sections = [] ;
		  structure = [] ;
		  lyrics = [] ;
		  outputs = [] ;
		  lilyponds = [] ;
		  tempo = 80 ;
		  path = path ;
		} in
		let songs = song::world.D.World.songs in
		let () = Rw.write_song song in
		let world = D.update_world_songs world songs in
		  text_page (sprintf "ok, %s is now valid" (strip_root world path)) 
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack e in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j ;
		  )
	    )
	  | s -> ( log "no such page : %s" s  ; page_404 s )
	      
    with
      | e -> (
	  let msg = Song_exn.html_string_of_stack () in
	  let msg2 = Song_exn.string_of_stack e in
	  let () = Song_exn.clear_stack () in
	  let (p,e0,e) = start_html_page () in
	  let pf fs = ksprintf p fs in
	    e0 () ;
	    pf "<h3> erreur : <br/></h3> " ;
	    pf "%s" msg ;
	    pf "</body></html>";
	    log "== END with error : %s"  msg2 ;
	    e () 
	)
	    
  in
  let _ = __SONG__try "register" (Callback.register "process_request" (process_request)) in

  let init () = () in

  let _ = __SONG__try "register" (Callback.register "web_init" init) in

    Fcgi.c_loop ()
)


  
let _ = try
    eprintf "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\n" ;
  (*
    argv(1) : la ou sont les sources des chansons
    argv(2) : la ou seront generees les pages html
    argv(3) : le doc_root du server web
    argv(4) : root_path : le path url vers le docroot (en general vide)
  *)
  (*
    output_dir : la ou seront generees les pages html
    doc_root : le doc_root du server web
    relative_output_dir : la ou seront generes les pages html, en url
    root_path : le path du doc_root
  *)

  let cwd = Unix.getcwd () in

    let (output_root,doc_root,relative_output_root,root_path) = (
      let output_root = __SONG__try "output_dir" (Sys.argv.(2)) in
	(* let () = __SONG__try ("mkdir " ^ output_dir) (Util.mkdir output_dir) in *)
      let output_root = normalize_path (
	if Filename.is_relative output_root then cwd // output_root else output_root ) in
  
      let doc_root = __SONG__try "doc_root" (Sys.argv.(3))  in
      let doc_root = normalize_path ( 
	if Filename.is_relative doc_root then cwd // doc_root else doc_root  ) in
	
      let relative_output_root = 
	let a = Str.split (Str.regexp (Str.quote doc_root)) output_root in
	  match a with
	    | [a] -> a
	    | l -> (
		let s = sprintf "output_root = %s\ndoc_root = %s\n" output_root doc_root in
		let s = List.fold_left ( fun acc s -> sprintf "%s--> %s\n" acc s) s l 
		in __SONG__failwith ("could not forge relative_output_root " ^ s)
	      )
      in
	
      let root_path = if Array.length Sys.argv > 4 then Sys.argv.(4) else "" in
	
	output_root,doc_root,relative_output_root,root_path
    )
    in
    let root = Sys.argv.(1) in
    let root = normalize_path root in
    let () = if true then (
      eprintf "-- %s\n-- %s\n-- %s\n-- %s\n" doc_root relative_output_root root_path root
    ) else () in
      Fcgi.c_init () ;
      (* let _ = __SONG__failwith "ici"  in *)
      let (songs,errors) = Rw.all_songs_from_root root in
      (* let songs = List.map Generate.song_of_path songs in *)
      let world = {
	D.World.songs = songs ;
	errors = errors ;
	root = normalize_path root ;
	output_root = output_root ;
	doc_root = doc_root ;
	url_root = relative_output_root ;
	root_path = root_path
      } in
      let () = try 
	  D.update_world world 
	with
	  | e -> eprintf "%s\n" (Song_exn.string_of_stack e) 
      in
	main_loop ()  ;
	exit 33
  with
    | e -> 
	let () = eprintf "%s\n" (Song_exn.string_of_stack e) in
	let () = __SONG__print_exn_stack e in 
	  exit 1
					 
