open Printf
open Data
open Util

let (//) = Filename.concat 

type args = ( string * string ) list

let log = Fcgi.log

let page = ref ""

let start_page () =
  page := ""

let print s = page := !page ^ s ^ "\n"
let pf fs =  ksprintf ( fun s -> page := !page ^ s ^ "\n" ) fs

let end_page code mime = 
  (* Fcgi.log "%s" (!page) ; *)
  Fcgi.c_fcgi_print (sprintf "Status: %d\r\n" code) ;
  Fcgi.c_fcgi_print (sprintf "Content-type: %s \r\n" mime) ;
  Fcgi.c_fcgi_print (sprintf "Content-length: %d \r\n\r\n" (String.length !page)) ;
  Fcgi.c_fcgi_print !page ;
  page := "" ;
  ()

let end_html_page () = end_page 200 "text/html"
let end_json_page () = end_page 200 "text/json"

let page_404 s =
  start_page () ;
  pf  "<html><body>no such url : <br/>%s<br/></body></html>" s ;
  end_page 404 "text/html" ;
  ()
    
let page_403 s =
  start_page () ;
  pf  "internal error : \n%s\n" s ;
  end_page 403 "text/plain" ;
  page := "" ;
  ()
    



let log = Fcgi.log


let plog = Fcgi.c_fcgi_log_string

let rec manage_all_songs   ~root ~output_dir ~doc_root ~relative_output_dir   ~root_path  songs root = __SONG__try "manage_all_songs" (
  (* pf "manage_all_songs, root = %s<br/>\n" root ;*)
  let dirs = Sys.readdir root in
  let dirs = Array.to_list dirs in
    
    List.fold_left ( fun songs d ->
		       let d = root//d in
		       let () = log "Reading directory %s" d in
			 if  Sys.is_directory d then (
			   if Filename.check_suffix d ".song" then (
			     try
			       let s = Generate.generate   ~root ~output_dir ~doc_root ~relative_output_dir ~root_path ~print ~plog:Fcgi.c_fcgi_log_string d in
				 s::songs
			     with
			       | e -> (
				   let msg = Song_exn.html_string_of_stack () in
				   let () = Song_exn.clear_stack () in
				     pf "<div class=\"error-msg\">" ;
				     pf "<hr/>" ;
				     pf  "<h3>Ooops... une erreur a été détectée à l'exécution</h3><br/>\n" ;
				     pf  "%s" msg ;
				     pf "<hr/></div>" ;
				     songs
				 )
			   )
			   else 
			     manage_all_songs ~root ~output_dir ~doc_root ~relative_output_dir  ~root_path  songs d
			 ) else (
			   songs
			 )
    ) songs dirs 
)
  
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
		start_page () ;
		pf "<html>" ;
		pf "<head>";
		pf "<style>
li.not_done {
font-style:italic;font-family:Georgia,Times,serif;font-size 1em ;
color:#bfe1f1 ;
} 

li.ok_done {
font-style:bold;font-family:Georgia,Times,serif;font-size 1em ;
color:#440044 ;
} 

li.ok_failed {
font-style:bold;font-family:Georgia,Times,serif;font-size 1em ;
color:#ff0000 ;
} 

li.error {
background-color : #ffcccc ;
border: 1px solid #000000 ;
list-style: none ;
}


</style>" ;
		pf "<script src=\"js/jquery-1.9.1.js\"></script>";
		pf "<script src=\"js/jquery-ui-1.10.1.custom.js\"></script>" ;
		pf "<script>

" ;

		pf "to_go = %d ; " (List.length song_paths) ;
		pf "

var nb_errors = 0 ;
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
	$('#results').prepend(\"<li>\"+data.name+\"</li>\") ;
        if (data.success == true) {
          $('#song-'+data.index).attr('class','ok_done') ;
          songs[data.index] = data.song ;
        } else {
          $('#song-'+data.index).attr('class','ok_failed') ;
          nb_errors = nb_errors + 1 ;
          $('#nb-errors').text(nb_errors + ' error(s)') ;
          $('#errors').append('<li class=\"error\"><pre>'+data.msg+'</pre></li>') ;
        }
        to_go = to_go-1 ;
        if (to_go>0) {   $('#status').text(to_go + ' restant a faire') ; }
        else {  
           update_index() ;
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
		List.iter ( fun (s,i) -> pf "<li id='song-%d' class='not_done'> %s </li>" i s ) song_paths ;
		
		pf "</ul>
<div id='show_end'></div> 
</div> " ;
		pf "<div id='status'>%d restant a faire</div>" (List.length song_paths) ;
		pf "<div id='index'><a href='%s/index.html'>index</a></div>" relative_output_dir ;
		pf "<p id='nb-errors'>0 errors</p>" ;
		pf "<ul id='errors'/>" ;
		pf "<ul id='message'/>" ;

		
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
		      let song = Generate.generate ~root ~output_dir ~doc_root ~relative_output_dir ~root_path ~plog ~print (List.assoc "path" params) in
			world := { World.songs = song :: (!world).World.songs } ;
			(* Index.write_index ~songs:!world.World.songs ~root_path ~output_dir ~relative_output_dir ; *)
			start_page ( ) ;
			let j = Json_type.Build.objekt [ 
			  "path",Json_type.Build.string path ;
			  "index",Json_type.Build.int index ;
			  "success",Json_type.Build.bool true ;
			  "song",Data_j.Song.to_json song ;
			] in
			  pf "%s"(Json_io.string_of_json j) ;
			  end_json_page ()
		    with
		      | e ->
			  start_page() ;
			  let msg = Song_exn.string_of_stack() in
			  let () = Song_exn.clear_stack () in
			  let j = Json_type.Build.objekt [ 
			    "path",Json_type.Build.string path ;
			    "index",Json_type.Build.int index ;
			    "success",Json_type.Build.bool false ;
			    "msg",Json_type.Build.string msg ;
			  ] in
			    pf "%s" (Json_io.string_of_json j) ;
			    end_json_page ()
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
		  pf "%s" (Json_io.string_of_json j) ;
		  end_json_page()
	      with
		| e -> (
		    let msg = Song_exn.string_of_stack () in
		    let () = Song_exn.clear_stack () in
		    let j = Json_type.Build.objekt [
		      "success",Json_type.Build.bool false ;
		      "msg",Json_type.Build.string msg ;
		    ] in
		      pf "%s" (Json_io.string_of_json j) ;
		      end_json_page();
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
    let (output_dir,doc_root,relative_output_dir,root_path) = (
      let output_dir = __SONG__try "output_dir" (Sys.argv.(2)) in
	(* let () = __SONG__try ("mkdir " ^ output_dir) (Util.mkdir output_dir) in *)
  
      let doc_root = __SONG__try "doc_root" (Sys.argv.(3)) in
	
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
					 
