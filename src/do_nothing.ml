open Printf
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
let pf5 fs s1 s2 s3 s4 s5 = let s = sprintf fs s1 s2 s3 s4 s5 in page := !page ^ s
let pf6 fs s1 s2 s3 s4 s5 s6 = let s = sprintf fs s1 s2 s3 s4 s5 s6 in page := !page ^ s

let log fs s =
  Fcgi.fcgi_log (sprintf fs s)


  
let main_loop  ()  = __SONG__try "main loop" (
  Fcgi.fcgi_log "test fcgi_log" ;
  
  let process_request () = 
    try
      start_page () ;
      pf0 "<h3>hello world</h3>\n" ;
      end_page() ;
    with
      | e -> (
	  pf0 (Song_exn.html_string_of_stack()) ;
	  end_page ()
	)
	    
  in
  let _ = __SONG__try "register" (Callback.register "process_request" (process_request)) in

  let init () = () in

  let _ = __SONG__try "register" (Callback.register "web_init" init) in

    Fcgi.c_loop ()
)


  
let _ = try
    Fcgi.c_init () ;
    main_loop () ;
      exit 33
  with
    | e -> let () = __SONG__print_exn_stack e in exit 1
