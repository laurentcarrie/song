open Printf
open Data
open Util
open ExtString
open ExtList

let (//) = Filename.concat 

type args = ( string * string ) list

let log = Fcgi.log




let log = Fcgi.log


let plog = Fcgi.c_fcgi_log_string

let main_loop  ~root ~output_dir ~doc_root ~relative_output_dir ~root_path = __SONG__try "main loop" (
  log "test fcgi_log" ;
  log "root is %s" root ;
  Array.iter ( fun a ->
    log "arg : %s" a
  ) Sys.argv ;

  (
    Array.iter ( fun s ->
      log "env :%s" s
    ) (Unix.environment())
  ) ;

  log "output_dir = %s" output_dir ; 
  log "doc_root = %s" doc_root ;
  log "relative_output_dir = %s" relative_output_dir ; 
  log "root_path = %s" root ;

  (*
    let print_404 script_name =
    let data = sprintf "<html><body><h3>url not found : </br><pre>%s</pre></h3></body></html>" script_name in
    Fcgi.c_fcgi_print (sprintf "Status: 404\r\n") ;
    Fcgi.c_fcgi_print (sprintf "Content-type: text/html \r\n" ) ;
    Fcgi.c_fcgi_print (sprintf "Content-length: %d \r\n\r\n" (String.length data)) ;
    Fcgi.c_fcgi_print data ;
    ()
    in
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
	      world := { World.songs = [] } ;
	      let song_paths = Generate.find_all_songs ~plog:Fcgi.c_fcgi_log_string root in 
		start_html_page () ;
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
		let stripped_song_paths = List.map ( fun (s,i) -> Str.global_replace (Str.regexp (Str.quote output_dir)) "" s,i) song_paths in
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
<body>
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
		pf "<div id='index'><a href='%s/index.html'>index</a></div>" relative_output_dir ;
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
		  pf "</body></html>";
		  log "== END script : %s"  script_name ;
		  end_html_page() ;
	    ) 
	      
	  | "/generate-song.songx" -> (
	      let params = Fcgi.get_post_params () in
		List.iter ( fun (name,value) ->
		  log "POST return : %s -> %s" name value
		) params ; 
		try
		  let path = (List.assoc "path" params) in
		  let index = int_of_string (List.assoc "index" params) in (
		    try 
		      let (song,generated) = Generate.generate ~root ~output_dir ~doc_root ~relative_output_dir ~root_path ~plog ~print (List.assoc "path" params) in
			world := { World.songs = song :: (!world).World.songs } ;
			(* Index.write_index ~songs:!world.World.songs ~root_path ~output_dir ~relative_output_dir ; *)
			start_page () ;
			let j = Json_type.Build.objekt [ 
			  "path",Json_type.Build.string path ;
			  "index",Json_type.Build.int index ;
			  "success",Json_type.Build.bool true ;
			  "song",Data_j.Song.to_json song ;
			  "generated",Json_type.Build.bool generated ;
			] in
			  json_page j
		    with
		      | e ->
			  let msg = Song_exn.string_of_stack() in
			  let () = Song_exn.clear_stack () in
			  let j = Json_type.Build.objekt [ 
			    "path",Json_type.Build.string path ;
			    "index",Json_type.Build.int index ;
			    "success",Json_type.Build.bool false ;
			    "msg",Json_type.Build.string msg ;
			  ] in
			    json_page j
		  )
		with
		  | e ->
		      let msg = Song_exn.string_of_stack() in
		      let () = Song_exn.clear_stack () in
			page_403 msg
	    )
	  | "/update_index.songx" -> ( 
	      try
		(* 
		   let params = Fcgi.get_post_params () in
		   let songs = __SONG__try "param songs" (List.assoc "songs" params) in
		   let j = __SONG__try "j" (Json_io.json_of_string songs) in
		   let songs = __SONG__try "of_json" (Json_type.Browse.list Data_j.Song.of_json j) in
		*)
		log "entering /update_index"  ;
		log "world has %d songs" (List.length !world.World.songs) ;
		Index.write_index ~songs:!world.World.songs ~root_path ~output_dir ~relative_output_dir ;
		start_page () ;
		let j = Json_type.Build.objekt [
		  "success",Json_type.Build.bool true ;
		  "nb_songs",Json_type.Build.int (List.length !world.World.songs) ;
		] in
		  json_page j
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack () in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      json_page j
		  )
	    )
	  | "/edit.songx" 
	  | "/edit_song.songx" -> ( 
	      try
		let params = Fcgi.parse_query_string () in
		let path = __SONG__try "path" ( List.assoc "path" params) in
		let song = Generate.song_of_path path in
		  Edit.render song relative_output_dir
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack () in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      pf "%s" (Json_io.string_of_json j) ;
		      end_html_page();
		  )
	    )
	  | "/internal-edit.songx" -> ( 
	      try
		let params = Fcgi.get_post_params() in
		let () = log "NBPARAMS : %d"( List.length params) in
		let () = List.iter ( fun (key,_) -> log "KEY : %s" key ) params in
		let value = __SONG__try "get value" (List.assoc "b" params) in
		  log "VALUE : %s" value ;
		  (* let value = List.fold_left ( fun acc c -> acc ^ "%") "" (String.explode value) in*)
		  (*
		  let value = "%20" in 
		    text_page value*)
		  let decoded = ( __SONG__try "decode"(Base64.str_decode value)) in
		    start_page () ;
		    pf "%s" decoded ; 
		  end_text_page () ;
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack () in
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
	  let msg2 = Song_exn.string_of_stack () in
	  let () = Song_exn.clear_stack () in
	  let () = start_page () in
	    pf "<h3> erreur : <br/></h3> " ;
	    pf "%s" msg ;
	    pf "</body></html>";
	    log "== END with error : %s"  msg2 ;
	    end_html_page ()
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

    let (output_dir,doc_root,relative_output_dir,root_path) = (
      let output_dir = __SONG__try "output_dir" (Sys.argv.(2)) in
	(* let () = __SONG__try ("mkdir " ^ output_dir) (Util.mkdir output_dir) in *)
      let output_dir = normalize_path (
	if Filename.is_relative output_dir then cwd // output_dir else output_dir ) in
  
      let doc_root = __SONG__try "doc_root" (Sys.argv.(3))  in
      let doc_root = normalize_path ( 
	if Filename.is_relative doc_root then cwd // doc_root else doc_root  ) in
	
      let relative_output_dir = 
	let a = Str.split (Str.regexp (Str.quote doc_root)) output_dir in
	  match a with
	    | [a] -> a
	    | l -> (
		let s = sprintf "output_dir = %s\ndoc_root = %s\n" output_dir doc_root in
		let s = List.fold_left ( fun acc s -> sprintf "%s--> %s\n" acc s) s l 
		in __SONG__failwith ("could not forge relative_output_dir " ^ s)
	      )
      in
	
      let root_path = if Array.length Sys.argv > 4 then Sys.argv.(4) else "" in
	
	output_dir,doc_root,relative_output_dir,root_path
    )
    in
    let root = Sys.argv.(1) in
    let () = if true then (
      eprintf "-- %s\n-- %s\n-- %s\n-- %s\n" doc_root relative_output_dir root_path root
    ) else () in
      Fcgi.c_init () ;
      main_loop ~output_dir ~root ~doc_root ~relative_output_dir ~root_path ;
      exit 33
  with
    | e -> let () = 
	eprintf "%s\n" (Song_exn.string_of_stack ()) ;
	     __SONG__print_exn_stack e in exit 1
					 
