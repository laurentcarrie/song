open Data
open Printf
open ExtString

let notes = [ "A";"B";"C";"D";"E";"F";"G" ]

let int_of_string s =
  try int_of_string s with | e -> failwith("cannot convert '"^s^"' to an int")

let chord_of_string s = __SONG__try ("note_of_string '" ^ s ^ "'") (
  let s = String.explode s in
  let (letter,s) = 
    match s with
      | []  -> __SONG__failwith "empty note ?"
      | hd::tl -> hd,tl
  in
  let (is_flat,s) =
    match s with
      | 'b'::tl -> true,tl
      | l -> false, l
  in
  let (is_sharp,s) =
    match s with
      | '#'::tl -> true,tl
      | l -> false, l
  in
  let (is_m,s) =
    match s with
      | 'm'::tl -> true,tl
      | l -> false, l
  in
  let (is_7,is_7M,s) = 
    match s with
      | '7'::'M':: tl -> false,true,s
      | '7'::tl -> true,false,s
      | l -> false,false,s
  in
  let (duration,s) =
    match s with
      | [] -> 4,s
      | '\\'::c:: tl -> (int_of_string (String.of_char c)),tl
      | _ -> __SONG__failwith "invalid note (duration?)"
  in
  let () = match s with
    | [] -> ()
    | l -> __SONG__failwith ("invalid note, unparsed : '" ^ (String.implode l) ^ "'")
  in
  let note = { Data.Note.letter=letter ; is_7=is_7 ; is_7M = is_7M ; is_m = is_m ; is_flat = is_flat ; is_sharp = is_sharp } in
    { Data.Chord.note = note ; length=duration }
)
let read_file song filename = __SONG__try "Grille_of_file.read_file" (
  let fin = open_in_bin filename in
  let rec read acc current linecount =
    try
      let line  = input_line fin in
	match current,line with
	  | None,"" -> read acc None (linecount+1)
	  | None,s -> read acc (Some { Data.Section.name=s;signature=4;chords=[] }) (linecount+1)
	  | Some current,"" -> read (PMap.add current.Data.Section.name current acc) None (linecount+1)
	  | Some current,line -> (
	      let words = Str.split (Str.regexp "[ \t]+") line in
	      let chords = __SONG__try ("file " ^ filename ^ ", line " ^ (string_of_int linecount))
		(List.map ( fun word ->  Section.C (chord_of_string word) ) words) in
	      let chords = chords @ [ Section.NL ] in
	      let current = { current with Data.Section.chords = current.Data.Section.chords @ chords } in
		read acc (Some current) (linecount+1)
	    )
    with
      | End_of_file -> close_in fin ; 
	  match current with
	    | None -> acc
	    | Some current -> PMap.add current.Data.Section.name current acc
  in
  let data = read (PMap.create String.compare) None 1 in
    PMap.iter ( fun _ v -> 
      printf "section %S\n" v.Data.Section.name ;
      List.iter ( fun n ->
	match n with
	  | Section.NL -> printf "\n"
	  | Section.C n ->
	      printf "%S, %d\n" (Data.Note.name n.Data.Chord.note) n.Data.Chord.length
      ) v.Data.Section.chords
    ) data ;
    { song with Song.sections = data }
)
