open ExtString
open Printf

let codes =
  [
    " ","%20" ;
    "!","%21" ;
    "\"","%22" ;
    "#","%23" ;
    "$","%24" ;
    "%","%25" ;
    "&","%26" ;
    "'","%27" ;
    "(","%28" ;
    ")","%29" ;
    "*","%2A" ;
    "+","%2B" ;
    ",","%2C" ;
    "-","%2D" ;
    ".","%2E" ;
    "/","%2F" ;
    "0","%30" ;
    "1","%31" ;
    "2","%32" ;
    "3","%33" ;
    "4","%34" ;
    "5","%35" ;
    "6","%36" ;
    "7","%37" ;
    "8","%38" ;
    "9","%39" ;
    ":","%3A" ;
    ";","%3B" ;
    "<","%3C" ;
    "=","%3D" ;
    ">","%3E" ;
    "?","%3F" ;
    "@","%40" ;
    "A","%41" ;
    "B","%42" ;
    "C","%43" ;
    "D","%44" ;
    "E","%45" ;
    "F","%46" ;
    "G","%47" ;
    "H","%48" ;
    "I","%49" ;
    "J","%4A" ;
    "K","%4B" ;
    "L","%4C" ;
    "M","%4D" ;
    "N","%4E" ;
    "O","%4F" ;
    "P","%50" ;
    "Q","%51" ;
    "R","%52" ;
    "S","%53" ;
    "T","%54" ;
    "U","%55" ;
    "V","%56" ;
    "W","%57" ;
    "X","%58" ;
    "Y","%59" ;
    "Z","%5A" ;
    "[","%5B" ;
    "\\","%5C" ;
    "]","%5D" ;
    "^","%5E" ;
    "_","%5F" ;
    "`","%60" ;
    "a","%61" ;
    "b","%62" ;
    "c","%63" ;
    "d","%64" ;
    "e","%65" ;
    "f","%66" ;
    "g","%67" ;
    "h","%68" ;
    "i","%69" ;
    "j","%6A" ;
    "k","%6B" ;
    "l","%6C" ;
    "m","%6D" ;
    "n","%6E" ;
    "o","%6F" ;
    "p","%70" ;
    "q","%71" ;
    "r","%72" ;
    "s","%73" ;
    "t","%74" ;
    "u","%75" ;
    "v","%76" ;
    "w","%77" ;
    "x","%78" ;
    "y","%79" ;
    "z","%7A" ;
    "{","%7B" ;
    "|","%7C" ;
    "}","%7D" ;
    "~","%7E" ;
    " ","%7F" ;
    "`","%80" ;
    " ","%81" ;
    ",","%82" ;
    "f","%83" ;
    "'","%84" ;
    " ","%85" ;
    "+","%86" ;
    "+","%87" ;
    "^","%88" ;


    "è","%E8" ;
    "é","%E9" ;
    "ê","%EA" ;
  ]
type status =
    | Normal
    | First
    | Second of char

let i_of_letter c = 
  match c with
    | '0' -> 0x00 
    | '1' -> 0x01 
    | '2' -> 0x02 
    | '3' -> 0x03 
    | '4' -> 0x04 
    | '5' -> 0x05 
    | '6' -> 0x06 
    | '7' -> 0x07 
    | '8' -> 0x08 
    | '9' -> 0x09 
    | 'A' -> 0x0A 
    | 'B' -> 0x0B 
    | 'C' -> 0x0C 
    | 'D' -> 0x0D 
    | 'E' -> 0x0E 
    | 'F' -> 0x0F
    | c    -> failwith ("bad letter : " ^ (Char.escaped c))

(* conversion codage url -> utf8 *)
let from_url s = 
  let rec loop status chars buf count = match chars with
    | [] -> Buffer.contents buf
    | '+'::chars -> (
	Buffer.add_char buf ' ' ;
	loop status chars buf (count+1)
      )
    | c::chars -> ( match c with
	| '%' -> (
	    match status with
	      | Normal -> loop First chars buf (count+1)
	      | _ -> assert(false)
	  )
	| c  -> ( match status with
	    | Normal -> Buffer.add_char buf c; loop Normal chars buf (count+1)
	    | First  -> loop (Second c) chars buf (count+1)
	    | Second u1 -> (* loop (Third (u1,c)) chars buf (count+1) *)
		(
		  let uu = 16*(i_of_letter u1) + (i_of_letter c) in
		    Buffer.add_char buf (Char.chr uu) ; 
		    loop Normal chars buf (count+1)
		)
	  )
      )
  in
    loop Normal (String.explode s) (Buffer.create 512) 0
      
      
      
(*

let f ?encoding (src : [`Channel of in_channel | `String of string]) =

  let rec loop d buf count = match Uutf.decode d with 
    | `Uchar u -> Uutf.Buffer.add_utf_8 buf u; loop d buf (count+1)
    | `End -> Buffer.contents buf 
    | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep; loop d buf  (count+1)
    | `Await -> assert false
  in
  let nln = `Readline 0x000A in
    loop  (Uutf.decoder ~nln ?encoding src) (Buffer.create 512) 0
*)


let to_url s = 
  List.fold_left ( fun acc c ->
    sprintf "%s%%%02x" acc (Char.code c)
  ) "" (String.explode s)

