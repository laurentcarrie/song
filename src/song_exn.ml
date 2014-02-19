
open ExtList
open Printf
let msg_stack = ref [] ;;

let push_message filename line msg =
  msg_stack := (filename,line,msg) :: !msg_stack 
;;

let push_message_fmt filename line fs =
  let f msg = 
    msg_stack := (filename,line,msg) :: !msg_stack  ;
    ()
  in
    ksprintf f fs
;;
    
let clear_stack () =
  msg_stack := []
;;

let print_stack () =
  printf "stack of size %d\n" (List.length !msg_stack) ;	
  List.iteri ( fun i (filename,line,msg) -> 
    printf "%2d : %s:%d -> %s\n" i filename line msg ) (List.rev !msg_stack) ;
  flush stdout
;;
    

let string_of_stack () =
  let (_,ret) = List.fold_left ( fun (i,acc) (filename,line,msg) -> 
    i+1,sprintf "%s%2d : %s:%d -> %s\n" acc i filename line msg 
  ) (0,sprintf "stack of size %d\n" (List.length !msg_stack)) (List.rev !msg_stack) in
    ret
;;
    
let html_string_of_stack () =
  let (_,ret) = List.fold_left ( fun (i,acc) (filename,line,msg) -> 
    i+1,sprintf "%s%2d : %s:%d -> %s<br/>" acc i filename line msg 
  ) (0,sprintf "stack of size %d<br/>" (List.length !msg_stack)) (List.rev !msg_stack) in
    ret
;;
    

let ref_level = ref 0 ;;
let ref_debug = ref false

let set_debug b = ref_debug := b ;;


let get_stack () = !msg_stack ;;
