
open Camlp4.PreCast ;;
open Syntax ;;
open Printf ;;


let expand_filename loc =
  let filename = Loc.file_name loc in
    <:expr< $str:filename$ >>
;;

let expand_here loc =
  let filename = Loc.file_name loc in
  let line = Loc.start_line loc in
  let line = string_of_int line in 
    <:expr< (
      Printf.printf "%s:%d\n" $str:filename$ $int:line$  ;
      flush stdout
    ) >> 
;;



let expand_line loc =
  let line = Loc.start_line loc in
  let line = string_of_int line in 
    <:expr< $int:line$ >>
;;

let expand_debug loc e =
  let e = <:expr< $e$>> in
    <:expr< Song_exn.set_debug ($e$) >>
;;


let expand_failwith loc msg =
  let filename = Loc.file_name loc in
  let line = string_of_int (Loc.start_line loc) in
    <:expr<
      let () = Song_exn.push_message $str:filename$ $int:line$ $msg$ in
	failwith $msg$
	  >>
;;

let expand_not_implemented loc  =
  let filename = Loc.file_name loc in
  let line = string_of_int (Loc.start_line loc) in
    <:expr<
      let () = Song_exn.push_message $str:filename$ $int:line$ "NOT IMPLEMENTED" in
	failwith "NOT IMPLEMENTED"
	  >>
;;

let expand_failwith_fmt loc msg =
  let filename = Loc.file_name loc in
  let line = string_of_int (Loc.start_line loc) in
    <:expr<
      let () = Song_exn.push_message_fmt $str:filename$ $int:line$ $msg$ in
	failwith $msg$
	  >>
;;


let expand_tw loc msg what =
  let msg = <:expr< $msg$>> in
  let filename = Loc.file_name loc in
  let line = string_of_int (Loc.start_line loc) in
    <:expr<
      try
	let ret = ($what$) in
	  ret
      with
	  [ e -> (
	    let () = Song_exn.push_message $str:filename$ $int:line$ $msg$ in raise(e)) ]
	  >>
;;

let expand_tw_fmt loc msg fs what =
  let msg = <:expr< $msg$>> in
  let filename = Loc.file_name loc in
  let line = string_of_int (Loc.start_line loc) in
    <:expr<
      try
	let ret = ($what$) in
	  ret
      with
	  [ e -> (let () = Song_exn.push_message_fmt $str:filename$ $int:line$ $msg$ in raise(e)) ]
	  >>
;;

(*
let expand_return loc msg what =
  let msg = <:expr< $msg$>> in
  let filename = Loc.file_name loc in
  let line = Loc.start_line loc in
    <:expr<
      let ret = ($what$) in
      let ret = Song.test.return ~label:$msg$ ~filename:$str:filename$ ~line:$int:line$  ret in
	ret
	  >>
;;
*)

let expand_raise loc e =
  let filename = Loc.file_name loc in
  let line = string_of_int (Loc.start_line loc) in
    <:expr<
      (let () = Song_exn.push_message $str:filename$ $int:line$ "raise" in raise($e$)) 
>>
;;

let expand_try_noraise loc msg what default print =
  let msg = <:expr< $msg$>> in
  let print = <:expr< $print$>> in
  let filename = Loc.file_name loc in
  let line = string_of_int (Loc.start_line loc) in
    <:expr<
      try
	let ret = ($what$) in
	  ret
      with
	  [ e -> (
	    let () = $print$ (Printf.sprintf "INITIAL ERROR: %s\n" (Printexc.to_string e)) in     
	    let () = Song_exn.push_message $str:filename$ $int:line$ $msg$ in 
	    let () = $print$ (Song_exn.string_of_stack ()) in
	    let () = Song_exn.clear_stack() in 
	      ($default$)) ]
	  >>
;;

let expand_exn_msg loc msg =
  let msg = <:expr< $msg$>> in
  let filename = Loc.file_name loc in
  let line = string_of_int (Loc.start_line loc) in
    <:expr<
      let _ = Song_exn.push_message $str:filename$ $int:line$ $msg$ in ()
    >>
;;

(*      (let () = Song_exn.push_message $str:filename$ $int:line$ $msg$ in ()) *)


let expand_print_stack loc e =
  let filename = Loc.file_name loc in
  let line = string_of_int (Loc.start_line loc) in
  let e = <:expr< $e$>> in
    <:expr< (
      let () = Song_exn.push_message $str:filename$ $int:line$ "print stack" in 
      let () = Printf.printf "INITIAL ERROR: %s\n" (Printexc.to_string $e$) in     
      let () = Song_exn.print_stack () in 
      let () = Song_exn.clear_stack() in
      let () = flush stdout  in ()
    )
>>
;;

(*      [ e -> (let () = Song_exn.push_message $str:filename$ $int:line$ (Printexc.to_string e) in Song_exn.print_stack ())] *)
  

EXTEND Gram

  GLOBAL: expr ;

(*  patt: LEVEL "simple"
      [ [ "[%"; "]" -> <:patt< abc >>  (* <:patt< lazy BatLazyList.Nil >> ] ; *) ] ] ;
  expr: LEVEL "::"
      [ [ p1 = SELF; "%::"; p2 = SELF ->
        <:patt< abc >>  (* <:patt< lazy BatLazyList.Cons($p1$,$p2$) >>*)  ] ] ;
*)
  expr: LEVEL "simple" [
    [ "__SONG__HERE__" ->  expand_here loc ]
  | [ "__SONG__NOT_IMPLEMENTED__" ->  expand_not_implemented loc ]
  | [ "__SONG__FILE__" ->  expand_filename loc ]
  | [ "__SONG__LINE__" ->  expand_line loc ] 
  | [ "__SONG__debug__" ; e = SELF ->  expand_debug loc e ] 
  | [ "__SONG__print_exn_stack"  ; e = SELF  ->   expand_print_stack loc e ]
  | [ "__SONG__failwith" ;  m = SELF  -> expand_failwith loc m ]
  | [ "__SONG__failwith_fmt" ;  m = SELF  -> expand_failwith_fmt loc m ]
  | [ "__SONG__raise" ;  e = SELF  -> expand_raise loc e ]
  | [ "__SONG__exn_msg" ;  e = SELF  -> expand_exn_msg loc e ]
  | [ "__SONG__try" ;  msg = expr LEVEL "simple"  ; what = expr LEVEL "simple" -> expand_tw loc msg what ]
(*  | [ "__SONG__return" ;  msg = expr LEVEL "simple"  ; what = expr LEVEL "simple" -> expand_return loc msg what ] *)
  | [ "__SONG__try_noraise" ;  msg = expr LEVEL "simple"  ; what = expr LEVEL "simple"  ; default = expr LEVEL "simple" -> 
      expand_try_noraise loc msg what default <:expr<print_endline>> ]
  | [ "__SONG__try_noraise_f" ;  msg = expr LEVEL "simple"  ; f = expr LEVEL "simple"  ; what = expr LEVEL "simple" ;  default = expr LEVEL "simple" -> 
      expand_try_noraise loc msg what default f ]
  ] ;

(*  patt: LEVEL "simple" [[ "my_failwith" ; e1 = SELF -> expand_f loc e1 ]] ;*)

  END


