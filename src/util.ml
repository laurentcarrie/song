open Data
open Printf

let int_of_string s =
  try int_of_string s with | e -> failwith("cannot convert '"^s^"' to an int")



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




let page = ref ""
let print s = page := !page ^ s ^ "\n"
let pf fs =  ksprintf ( fun s -> page := !page ^ s ^ "\n" ) fs


let start_page () =
  page := ""
    
let start_html_page() = (
  start_page () ;
  pf "<html>" ;
  pf "<head>";
  pf "<meta http-equiv=\"content-type\" content=\"text/html; charset=utf-8\" />" ;
  pf "<style>
li.not_done {
font-style:italic;font-family:Georgia,Times,serif;font-size 1em ;
color:#bfe1f1 ;
} 

li.ok_generated {
font-style:bold;font-family:Georgia,Times,serif;font-size 2em ;
color:#330033 ;
} 

li.ok_unchanged {
font-style:bold;font-family:Georgia,Times,serif;font-size 1em ;
color:#888888 ;
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
  
  pf "<script>
$(document).ready(function() {
     $('.edit').editable('/internal-edit.songx',
{
  indicator : 'Sauvegarde...',
  tootip    : 'Cliquez pour éditer',
  submit    : 'Ok',
  cancel    : 'Annuler',
  type      : 'textarea',
  submitdata  : function (value,settings) {
      console.log(value) ;
      console.log(utf8_to_b64(value)) ;
      var binval = utf8_to_b64(value) ;
      binval = binval.replace(/=/gi, '');
      console.log(binval) ;
//      $('#bin').text(utf8_to_b64(binval)) ;
      return { b:binval , song:'song' } ;
  },
  data      : function(value, settings) {
      /* Convert <br> to newline. */
      var retval = value.replace(/<br[\\s\\/]?>/gi, '\\n');
//      var ret = new Object ;
//      ret.value = retval ;
//      ret.song = 'xxx';
      return retval ;
  }
}
);
 });
</script>" ;
)

let end_page code mime = 
  (* Fcgi.log "%s" (!page) ; *)
  Fcgi.c_fcgi_print (sprintf "Status: %d\r\n" code) ;
  Fcgi.c_fcgi_print (sprintf "Content-type: %s \r\n" mime) ;
  Fcgi.c_fcgi_print (sprintf "Content-length: %d \r\n\r\n" (String.length !page)) ;
  Fcgi.c_fcgi_print !page ;
  page := "" ;
  ()

let end_html_page () = end_page 200 "text/html"
let end_text_page () = end_page 200 "text/plain"

let json_page j =
  start_page() ;
  pf "%s" (Json_io.string_of_json j) ;
  end_page 200  "text/json"

let text_page text =
  start_page() ;
  pf "%s" text ;
  end_page 200  "text/plain"
    
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
    
