open Printf
open Data
open Util

let (//) = Filename.concat 

type args = ( string * string ) list

let log = Fcgi.log

let page = ref ""

let start_page () =
  page := ""

let end_page mime = 
  (* Fcgi.log "%s" (!page) ; *)
  Fcgi.c_fcgi_print (sprintf "Status: 200\r\n") ;
  Fcgi.c_fcgi_print (sprintf "Content-type: %s \r\n" mime) ;
  Fcgi.c_fcgi_print (sprintf "Content-length: %d \r\n\r\n" (String.length !page)) ;
  Fcgi.c_fcgi_print !page ;
  page := "" ;
  ()

let end_html_page () = end_page "text/html"
let end_json_page () = end_page "text/json"

let page_404 s =
  start_page () ;
  Fcgi.c_fcgi_print (sprintf "<html><body>no such url : <br/>%s<br/></body></html>" s) ;
  Fcgi.c_fcgi_print (sprintf "Status: 404\r\n") ;
  Fcgi.c_fcgi_print (sprintf "Content-type: text/html \r\n" ) ;
  Fcgi.c_fcgi_print (sprintf "Content-length: %d \r\n\r\n" (String.length !page)) ;
  Fcgi.c_fcgi_print !page ;
  page := "" ;
  ()
    

let print s = page := !page ^ s ^ "\n"

let pf fs =  ksprintf ( fun s -> page := !page ^ s ^ "\n" ) fs


let log = Fcgi.log

  
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

	try

	  log "== BEGIN script : %s"  script_name ;
	  start_page () ;
	  pf "<html>" ;
	  pf "<head>"; 
	  pf "<script src=\"js/jquery-1.9.1.js\"></script>";
	  pf "<script src=\"js/jquery-ui-1.10.2.js\"></script>" ;
	  pf "<script>

function add_result(data) {
	$('#results').prepend(\"<li>\"+data.name+\"</li>\") ;
} ;

function add_error(msg) {
	$('#errors').prepend(\"<li>msg<li>\") ;
} ;

function show_end() {
        $('#show_end').prepend(\"fini !\") ;
}  ;

function manage(module) {
console.log(module) ;
$('#status').text = 'generation de : '+ module ;
$.ajax({
    type: 'POST',
    url:  'generate-song.songx',
    data: {path:module},
    success: function(data) { console.log('success '+ data) ; add_result(data) ; },
    error: function(xhr,ajaxOptions,thrownError) {console.log('error '+ xhr.responseText) ; }
}) ;
} ;

$(document).ready(function () { 
"; 
	  List.iter ( fun s -> pf "manage('%s') ;" s) (Generate.find_all_songs ~plog:Fcgi.c_fcgi_log_string root) ;
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
<p id='status'</p>
<ul id='results'></ul>
<ul id='errors'></ul>
<div id='show_end'></div> 
</div>
"; 


	  let songs = 
	    (*manage_all_songs   ~root ~output_dir ~doc_root ~relative_output_dir  ~root_path  [] root in
	    *)
	    [] in
	    Index.write_index ~songs ~root_path ~output_dir ~relative_output_dir ;
	    pf "</body></html>";
	    log "== END script : %s"  script_name ;
	    end_html_page() 

	      
	with
	  | e -> (
	      let msg = Song_exn.html_string_of_stack () in
	      let msg2 = Song_exn.string_of_stack e in
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
	eprintf "%s\n" (Song_exn.string_of_stack e) ;
	     __SONG__print_exn_stack e in exit 1
					 
