external c_init : unit -> unit = "fcgi_init"
external c_loop : unit -> unit = "fcgi_loop"  
external c_fcgi_print : string -> unit = "fcgi_print"  

external c_fcgi_log_string : string -> unit = "fcgi_log_string"  

external fcgi_input : int -> string  = "fcgi_input"  
external fcgi_set_exit_status : int -> unit = "fcgi_set_exit_status"

open Printf

exception Http_404

open ExtString
open Util

let parse_query_string  () = __SONG__try "parse_query_string" (
  let s = Unix.getenv "QUERY_STRING" in
  let args = String.nsplit s "&"  in
    List.map ( fun a ->
      let l = String.nsplit a "=" in
	match l with
	  | name::[] -> (name,"")
	  | name::value::[] -> (name,Url_encode.from_url value)
	  | _ -> failwith "split failed"
    ) args
)
    
let get_post_params () = __SONG__try "get_post_params" (
  let length = __SONG__try "int_of_string" (int_of_string (__SONG__try "content_length" (Unix.getenv "HTTP_CONTENT_LENGTH"))) in 
  let header = fcgi_input length in
  let header = String.nsplit header "&" in
    List.map ( fun pair -> 
      match String.nsplit pair "=" with
	| name::[] -> name,""
	| name::value::[] -> name,Url_encode.from_url  value
	| _ -> __SONG__failwith("could not split " ^ pair)
    ) header
)

let pf fs = ksprintf c_fcgi_log_string  fs

let log fs = 
  pf fs

