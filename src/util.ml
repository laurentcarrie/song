open Data
open Printf
open ExtString
let (//) = Filename.concat

let log = Fcgi.log

type page_t = 
    | On_line
    | Off_line

let int_of_string s =
  try int_of_string s with | e -> failwith("cannot convert '"^s^"' to an int")

let path_to_list p = __SONG__try ("path_to_list " ^ p) (
  if Filename.is_relative p then __SONG__failwith "is_relative" else () ;
  let rec r current acc = 
    if current = "/" then acc
    else (
      r (Filename.dirname current) ((Filename.basename current)::acc)
    )
  in
  let l = r p [] in
  let (l,_) = List.fold_left ( fun (acc,skip) s ->
    match skip,s with
      | skip , "." -> acc,skip
      | false,".." -> acc,true
      | false,s    -> s::acc,false
      | true,".."  -> acc,true
      | true,s     -> acc,false
  ) ([],false) (List.rev l) in
    l
)

let normalize_path p = __SONG__try ("normalize_path " ^ p) (
  List.fold_left ( fun acc d -> acc // d ) "/" (path_to_list p))



let update_digest d s = __SONG__try "update_digest" (
  if not (Sys.file_exists s) then
    __SONG__failwith("no such file : " ^ s)
  else (
    let d2 = Digest.file s in
      Digest.string ( (Digest.to_hex d) ^ (Digest.to_hex d2))
  )
)

let mkdir dir = __SONG__try ("mkdir " ^ dir) (
  try 
    let s = Unix.stat dir in 
      match s.Unix.st_kind with
	| Unix.S_DIR -> ()
	| _ -> failwith(Printf.sprintf "could not create directory %s" dir)
  with
    | Unix.Unix_error(e,s1,s2) -> (
	match e with
	  | Unix.ENOENT -> (
	      try
		Unix.mkdir dir 0o770
	      with
		| e -> raise e
	    )
	  | _ -> __SONG__failwith((Unix.error_message e) ^ " ; " ^ s1 ^ " ; " ^ s2)
	      
      )
)

let open_in_bin filename =
  if not (Sys.file_exists filename) then
    __SONG__failwith ("no such file : " ^ filename )
  else
    open_in_bin filename

let open_out_bin filename = __SONG__try ("open_out_bin " ^ filename) (
  open_out_bin filename 
)


module Json = struct
  module Bu = Json_type.Build
  module Br = Json_type.Browse
    
  let string_field table name = __SONG__try ("string_field " ^ name) (
    Br.string (Br.field table name)
  )

  let int_field table name = __SONG__try ("int_field " ^ name) (
    Br.int (Br.field table name)
  )

end

module Edit_type = struct
  type t = 
      | Textarea
      | Select
      | Text

  let print ~loadurl p url song_path idtext field et = 
    let pf fs = ksprintf p fs in
      pf "<script>
$(document).ready(function() {
     $('#%s').editable('%s',
{
  indicator : 'Sauvegarde...',
  tootip    : 'Cliquez pour éditer',
"  idtext   url  ;
      pf "
  loadurl   : '%s',
" loadurl ;
      pf"
  submit    : 'Ok',
  cancel    : 'Annuler',
" ;
      (
	let (a,b) = match et with
	  | Textarea -> "textarea","textarea"
	  | Select -> "select","select" 
	  | Text -> "text","text"
	in
	  pf "
  type      : '%s',
  style     : 'editable-%s',
" a b 
      );
      
      pf "
  rows      : 20,
  loaddata  : function (value,settings) {
      return { 
        path:'%s',
        field:'%s'
    } ;
  },
  submitdata  : function (value,settings) {
      return { 
        path:'%s',
        field:'%s'
    } ;
  },
  data      : function(value, settings) {
      console.log('data : ' + value) ;
      return value ;
  }
}
);
 });
</script>" 
      (* load data *) song_path field
      (* submit data *) song_path field

end


   

let start_page code mime  =
  let page = ref "" in
  let print ?(nl=true) ?(space=true) s = 
    page := !page ^ s ;
    (if space then
      page := !page  ^ " "  
      else
	()
    ) ;
    ( if nl then
      page := !page ^ "\n"  
      else
	()
    ) ;
    ()
  in
  let end_page () = 
    Fcgi.c_fcgi_print (sprintf "Status: %d\r\n" code) ;
    Fcgi.c_fcgi_print (sprintf "Content-type: %s \r\n" mime) ;
    Fcgi.c_fcgi_print (sprintf "Content-length: %d \r\n\r\n" (String.length !page)) ;
    Fcgi.c_fcgi_print !page ;
    page := "" ;
    ()
  in
    print,end_page


let start_html_page () = (
  let (print,end_page) = start_page 200 "text/html" in
  let pf fs = ksprintf print fs in
  pf "<html>" ;
  pf "<head>";
  pf "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />" ;
  pf "<link rel=\"stylesheet\" href=\"/css.songx\"/>" ;
  pf "<script src=\"js/jquery-1.9.1.js\"></script>";
  pf "<script src=\"js/jquery-ui-1.10.1.custom.js\"></script>" ;
  pf "<script src=\"js/jquery.jeditable.js\" type=\"text/javascript\" charset=\"utf-8\"></script>" ;
  pf "<script>
function utf8_to_b64( str ) {
  return window.btoa(unescape(encodeURIComponent( str )));
}

function b64_to_utf8( str ) {
  return decodeURIComponent(escape(window.atob( str )));
} 
</script>
";

  let end_head () =
    pf "
</head>
</body>
"
  in
  let end_page () =
    pf "
</body>
</html>
" ;
    end_page()
  
  in
    print,end_head,end_page
)

let json_page j =
  let (p,e) = start_page 200 "text/json" in
  let pf fs = ksprintf p fs in
  let s = Json_io.string_of_json j in
    log "send json : %s" s ;
    pf "%s" s ;
    e ()

let json_error_page j =
  let (p,e) = start_page 404 "text/json" in
  let pf fs = ksprintf p fs in
    pf "%s" (Json_io.string_of_json j) ;
    e ()

let text_page text =
  let (p,e) = start_page 200 "text/plain" in
  let pf fs = ksprintf p fs in
    pf "%s" text ;
    e () 
    
let page_404 s =
  let (p,e) = start_page 400 "text/html" in
  let pf fs = ksprintf p fs in
    pf  "<html><body>no such url : <br/>%s<br/></body></html>" s ;
    e ()
    
let page_403 s =
  let (p,e) = start_page 300 "text/plain" in
  let pf fs = ksprintf p fs in
    pf  "internal error : \n%s\n" s ;
    e ()
      
let strip_root world s = __SONG__try "strip_root" (
  String.strip ~chars:"/" (normalize_path (Str.global_replace (Str.regexp (Str.quote world.World.root)) "" s))
)
let error_page (exn:exn) = (
  let stack = Song_exn.get_stack () in
  let () = Song_exn.clear_stack () in
  let (p,h,e) = start_html_page () in
  (* let pf fs = ksprintf p fs in *)
  let pfnl fs = ksprintf ( fun s -> p s ; p "\n" ) fs in
    h () ;
    let (msg:string) = Printexc.to_string exn in
      pfnl "<p>%s</p>" msg ;
      pfnl "<table>" ;
      List.iter ( fun (file,line,msg) ->
	let msg = Str.global_replace (Str.regexp "\n") "<br/>" msg in
	  pfnl "<tr><td class='error-filename'>%s</td><td class='error-line'>%d</td><td class='error-msg'>%s</td></tr>\n" file line msg
      ) stack ;
      pfnl "</table>" ;
    e ()
)
