open Printf
open Data
open Util

let (//) = Filename.concat 

type args = ( string * string ) list

let log = Fcgi.log

let page = ref ""

let start_page () =
  page := ""

let end_page () = 
  Fcgi.log "%s" (!page) ;
  Fcgi.c_fcgi_print (sprintf "Status: 200\r\n") ;
  Fcgi.c_fcgi_print (sprintf "Content-type: text/html \r\n" ) ;
  Fcgi.c_fcgi_print (sprintf "Content-length: %d \r\n\r\n" (String.length !page)) ;
  Fcgi.c_fcgi_print !page ;
  page := "" ;
  ()

let pf fs =  ksprintf ( fun s -> page := !page ^ s ^ "\n" ) fs


let log = Fcgi.log

let manage_song   ~root ~output_dir ~doc_root ~relative_output_dir   ~root_path  dirname  = __SONG__try ("manage song : " ^ dirname) (
  let song = { Song.title="???" ; Song.auteur="???" ; format=None ; sections=PMap.create String.compare ; structure=[];lyrics=[];outputs=[];
	       digest=Digest.string ""} in
    try
      pf "lecture du repertoire %s<br/>" dirname ; 
      log "lecture du repertoire : '%s'" dirname ;
    let song = Grille_of_file.read_file song    (dirname // "grille.txt") in
    let song = Lyrics_of_file.read_file song    (dirname // "lyrics.txt") in
    let song = Structure_of_file.read_file song (dirname // "structure.txt") in
    let song = Main_of_file.read_file song      (dirname // "main.txt") in
    let song = Sortie_of_file.read_file song (dirname // "sortie.txt") in
    let song = Check.check song in
      (* Std.output_file ~filename:(dirname//"digest.txt") ~text:(Digest.to_hex song.Song.digest) ; *)
      pf "<!-- root_path = %s -->\n"  root_path ;
      pf "<!-- relative_output_dir = %s -->\n" relative_output_dir ;
      (
	let tm = Unix.localtime(Unix.time ()) in
	  pf "at %02d/%02d/%04d %02d:%02d:%02d</br>" 
	    tm.Unix.tm_mday (tm.Unix.tm_mon+1) (tm.Unix.tm_year+1900)
	    tm.Unix.tm_hour tm.Unix.tm_min tm.Unix.tm_sec
      ) ;
      Html.render_html song output_dir ;
      Lilypond.render song output_dir ;
      List.iter ( fun output ->
	pf "<!-- output.Output.filename = %s -->\n" output.Output.filename ;
	pf "écriture de <a href=\"%s%s/%s.html\">%s.html</a><br/>\n" root_path relative_output_dir output.Output.filename output.Output.filename
      ) song.Song.outputs ;
      song
	
  with
    | e -> (
	let msg = Song_exn.html_string_of_stack () in
	let () = Song_exn.clear_stack () in
	  pf  "<h3>Ooops... une erreur a &eacute;t&eacute detect&eacute;e a l'execution</h3><br>\n" ;
	  pf  "<hr>\n" ;
	  pf  "%s" msg ;
	  song
      )
)


let rec manage_all_songs   ~root ~output_dir ~doc_root ~relative_output_dir   ~root_path  songs root = __SONG__try "manage_all_songs" (
  (* pf "manage_all_songs, root = %s<br/>\n" root ;*)
  let dirs = Sys.readdir root in
  let dirs = Array.to_list dirs in
    
    List.fold_left ( fun songs d ->
		       let d = root//d in
		       let () = log "Reading directory %s" d in
			 if  Sys.is_directory d then (
			   if Filename.check_suffix d ".song" then (
			     let s = manage_song   ~root ~output_dir ~doc_root ~relative_output_dir ~root_path  d in
			       s::songs
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
      log "env :%s\n" s
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
	    Str.global_replace (Str.regexp ".songx") "" script_name
	with
	  | Not_found -> "noscript"
      in
	log "script : %s"  script_name ;
	start_page () ;
	pf "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />" ;
	let songs = manage_all_songs   ~root ~output_dir ~doc_root ~relative_output_dir  ~root_path  [] root in
	  Index.write_index ~songs ~root_path ~output_dir ~relative_output_dir ;
	  end_page() ;
    with
      | e -> (
	  let msg = Song_exn.html_string_of_stack () in
	  let () = Song_exn.clear_stack () in
	  let () = start_page () in
	    pf "<h3> erreur : <br/></h3> " ;
	    pf "%s" msg ;
	end_page ()
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
					 
