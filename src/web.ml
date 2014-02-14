open Printf
open Data
open Util

let (//) = Filename.concat 

type args = ( string * string ) list

let page = ref ""

let start_page () =
  page := ""

let end_page () = 
  Fcgi.fcgi_log (!page) ;
  Fcgi.c_fcgi_print (sprintf "Status: 200\r\n") ;
  Fcgi.c_fcgi_print (sprintf "Content-type: text/html \r\n" ) ;
  Fcgi.c_fcgi_print (sprintf "Content-length: %d \r\n\r\n" (String.length !page)) ;
  Fcgi.c_fcgi_print !page ;
  page := "" ;
  ()

let pf fs =  !page ^ sprintf fs

let pf0 s =  page := !page ^ s
let pf1 fs s = let s = sprintf fs s in page := !page ^ s
let pf2 fs s1 s2 = let s = sprintf fs s1 s2 in page := !page ^ s
let pf3 fs s1 s2 s3 = let s = sprintf fs s1 s2 s3 in page := !page ^ s
let pf4 fs s1 s2 s3 s4 = let s = sprintf fs s1 s2 s3 s4 in page := !page ^ s

let log fs s =
  Fcgi.fcgi_log (sprintf fs s)


let (output_dir,doc_root,relative_output_dir,root_path) = 
  try
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

    let root_path = Sys.argv.(4) in
      
      output_dir,doc_root,relative_output_dir,root_path
  with
    | e ->
	let () = __SONG__print_exn_stack e in
	  exit 1

let manage_song dirname  = __SONG__try ("manage song : " ^ dirname) (
  try
    pf1 "reading directory %s<br/>" dirname ; 
    log "reading song in : '%s'" dirname ;
    let song = { Song.title="???" ; Song.auteur="???" ; format=None ; sections=PMap.create String.compare ; structure=[];lyrics=[];outputs=[];
		 digest=Digest.string ""} in
    let song = Grille_of_file.read_file song    (dirname // "grille.txt") in
    let song = Lyrics_of_file.read_file song    (dirname // "lyrics.txt") in
    let song = Structure_of_file.read_file song (dirname // "structure.txt") in
    let song = Main_of_file.read_file song      (dirname // "main.txt") in
    let song = Sortie_of_file.read_file song (dirname // "sortie.txt") in
      Std.output_file ~filename:(dirname//"digest.txt") ~text:(Digest.to_hex song.Song.digest) ;
      Html.render_html song output_dir ;
      List.iter ( fun output ->
	pf4 "wrote <a href=\"%s%s/%s.html\">%s.html</a><br/>\n" root_path relative_output_dir output.Output.filename output.Output.filename
      ) song.Song.outputs
	
  with
    | e -> (
	let msg = Song_exn.html_string_of_stack () in
	let () = Song_exn.clear_stack () in
	  pf0  "<h3>Ooops... une erreur a &eacute;t&eacute detect&eacute;e a l'execution</h3><br>\n" ;
	  pf0  "<hr>\n" ;
	  pf0  msg ;
      )
)


let rec manage_all_songs root = __SONG__try "manage_all_songs" (
  (* pf "manage_all_songs, root = %s<br/>\n" root ;*)
  let dirs = Sys.readdir root in
  let dirs = Array.to_list dirs in
    
    List.iter ( fun d ->
      let d = root//d in
      let () = log "Reading directory %s" d in
      if  Sys.is_directory d then (
	if Filename.check_suffix d ".song" then manage_song d else manage_all_songs d
      ) else (
	()
      )
    ) dirs ;


)
  
let main_loop root = __SONG__try "main loop" (
  Fcgi.c_init () ;
  Util.print := Some (Fcgi.fcgi_log) ;
  Fcgi.fcgi_log "test fcgi_log" ;
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
  log "root_path = %s" root_path ;

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
    Fcgi.fcgi_log ("script : " ^ Unix.getenv "SCRIPT_NAME") ;
    let script_name = try 
	Str.global_replace (Str.regexp ".songx") "" (Unix.getenv "SCRIPT_NAME")
      with
	| Not_found -> "noscript"
    in
      Fcgi.fcgi_log ("script : " ^ script_name) ;
      start_page () ;
      manage_all_songs root ;
      end_page() ;
  in
  let _ = Callback.register "process_request" (process_request) in

  let init () = () in

  let _ = Callback.register "web_init" init in

    Fcgi.c_loop ()
)


  
let _ = try
    __SONG__HERE__ ;
      
  main_loop Sys.argv.(1) ;
  __SONG__HERE__ ;
  exit 3 
  with
    | e -> let () = __SONG__print_exn_stack e in exit 1
