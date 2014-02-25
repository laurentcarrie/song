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
