open Data
open Printf
open ExtString
let (//) = Filename.concat

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
      | false,".." -> acc,true
      | false,s    -> s::acc,false
      | true,".."  -> acc,true
      | true,s     -> acc,false
  ) ([],false) (List.rev l) in
    l
)

let normalize_path p = 
  List.fold_left ( fun acc d -> acc // d ) "/" (path_to_list p)



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




let print_edit p url song_path idtext field = 
  let pf fs = ksprintf p fs in
    pf "<script>
$(document).ready(function() {
     $('#%s').editable('%s',
{
  indicator : 'Sauvegarde...',
  tootip    : 'Cliquez pour éditer',
  submit    : 'Ok',
  cancel    : 'Annuler',
  type      : 'textarea',
  rows      : 20,
  submitdata  : function (value,settings) {
      console.log('SUBMIT : ' + value) ;
      var binval = utf8_to_b64(value) ;
      binval = binval.replace(/=/gi,'');
//    $('#bin').text(utf8_to_b64(binval)) ;
      return { 
       path:'%s',
       t:$('#%s-hidden').text(), 
       b:binval,
       field:'%s'
    } ;
  },
  data      : function(value, settings) {
      console.log('data : ' + value) ;
      retval = $('#%s-hidden').text() ;
      return retval ;
  }
}
);
 });
</script>" 
      idtext
      url
      song_path
      idtext
      field
      idtext




   

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
  pf "<style>

.edit {
background:#eeeeee ;
}

.done-hide-me {
visibility:hidden ;
display:none ;
}

.hide-me {
display:none ;
}

div.div-count {
}

p.div-title {
font-style:italic;font-family:Georgia,Times,serif;font-size:1.5em ;
color:#000000 ;
}

li.not_done {
font-style:italic;font-family:Georgia,Times,serif;font-size:1em ;
color:#bfe1f1 ;
} 


li.ok_generated {
font-style:bold;font-family:Georgia,Times,serif;font-size:1em ;
color:#330033 ;
} 
li.ok_unchanged {
font-style:bold;font-family:Georgia,Times,serif;font-size:1em ;
color:#888888 ;
} 

li.ok_failed {
font-style:bold;font-family:Georgia,Times,serif;font-size:1em ;
color:#ff0000 ;
} 

li.error {
background-color : #ffcccc ;
border: 1px solid #000000 ;
list-style: none ;
}

#chords td, #chords th 
{ 
font-size:1em;
border:1px solid #98bf21;
padding:2px 2px 2px 2px;
text-align:center;
height:50px ;
width:2cm ;
}

td.half-chords
{
width:1cm ;
}

#chords th 
{
font-size:1.1em;
text-align:left;
padding-top:5px;
padding-bottom:4px;
background-color:#A7C942;
color:#ffffff;
}
#chords tr.alt td 
{
color:#000000;
background-color:#EAF2D3;
}

span.note {
background-color:red
word-spacing:1px
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

