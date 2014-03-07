open Data
open Printf
open ExtString
open ExtList
open Util

let notes = [ "A";"B";"C";"D";"E";"F";"G" ]

let int_of_string s =
  try int_of_string s with | e -> failwith("cannot convert '"^s^"' to an int")

let chord_of_string s = __SONG__try ("note_of_string '" ^ s ^ "'") (
  log "note=%s" s ;
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
  let () = log "is_sharp=%b" is_sharp in
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
      | '\\'::c::[] -> (int_of_string (String.of_char c)),[]
      | '\\'::c::tl -> (int_of_string (String.of_char c)),tl
      | l -> __SONG__failwith ("invalid note (duration?) : " ^ (String.implode l))
  in
  let () = match s with
    | [] -> ()
    | l -> __SONG__failwith ("invalid note, unparsed : '" ^ (String.implode l) ^ "'")
  in
  let note = { Data.Note.letter=letter ; is_7=is_7 ; is_7M = is_7M ; is_m = is_m ; is_flat = is_flat ; is_sharp = is_sharp } in
    { Data.Chord.note = note ; length=duration }
)
let update_data song data = __SONG__try "Grille.read_data" (
  let data = Str.split (Str.regexp "\n") data in
  let rec read acc current linecount data =
    match data with
      | [] -> (
	  match current with
	    | None -> List.rev acc
	    | Some current -> List.rev (current::acc)
	)
      | line::tl -> (
	  match current,line with
	    | None,"" -> read acc None (linecount+1) tl
	    | None,s -> read acc (Some { Data.Section.name=s;signature=4;chords=[] }) (linecount+1) tl
	    | Some current,"" -> read (current::acc) None (linecount+1) tl
	    | Some current,line -> (
		let line = String.strip line in
		let groups = Str.split (Str.regexp (Str.quote "|")) line in
		let groups = List.fold_left ( fun acc group ->
		  let words = Str.split (Str.regexp "[ \t]+") group in
		  let chords = __SONG__try ("chords") 
		    (List.map ( fun word ->  chord_of_string word ) words) in
		    (Section.G chords)::acc
		) [] groups in
		let groups = List.rev groups in
		let chords = groups @ [ Section.NL ] in
		let current = { current with Data.Section.chords = current.Data.Section.chords @ chords } in
		  read acc (Some current) (linecount+1) tl
	      )
	)
  in
  let data = read [] None 1 data in
    { song with Song.sections = data }
)

let read_file song filename = __SONG__try "Grille_of_file.read_file" (
  let data = Std.input_file filename in
    update_data song data
)


let to_print print song = __SONG__try "to_string" (
  let pf fs = ksprintf print fs in
    List.iter ( fun section -> 
      pf "%s\n" section.Section.name ;
      List.iter ( fun c ->
	match c with
	  | Section.NL -> pf "\n" ;
	  | Section.G g -> (
	      List.iter ( fun c ->  pf "%s " (Note.text_name c.Chord.note c.Chord.length) ) g ;
	      pf " |"  ;
	    )
      ) section.Section.chords  ;
      pf "\n" ;
    ) song.Song.sections ;
)

let to_string song = __SONG__try "to_string" (
  let buf = Buffer.create 1024 in
  let () = to_print (Buffer.add_string buf) song  in
    Buffer.contents buf
)


let to_html_print print song = (
  let pf fs = ksprintf print fs in
  let rec print_chords l signature offset = 
    match l with
      | [] -> ()
      | Section.NL::[] -> (
	  let s = if offset mod signature <> 0 then " - " else "" in
	    pf "  <span class=\"note\">%s</span></td>\n</tr>\n" s ;
	)
      | Section.NL::tl -> 
	  let s = if offset mod signature <> 0 then " - " else "" in
	    pf "  <span class=\"note\">%s</span></td>\n</tr>\n<tr>\n" s ;
	    print_chords tl signature 0
      | (Section.G g)::tl ->
	  pf "<td>" ;
	  List.iter ( fun c -> pf "  <span class=\"note\">%s</span> "  (Note.html_name c.Chord.note c.Chord.length) ) g ;
	  print_chords tl signature offset
  in
    
  let print_section s = __SONG__try "print" (
    pf "<h2>%s</h2>\n" s.Section.name ;
    pf "<table id=\"chords\">\n<tr>\n" ;
    print_chords s.Section.chords s.Section.signature 0   ;
    pf "</table>\n" 
  )
  in
    List.iter ( fun s -> print_section s ) song.Song.sections
)


let to_html song = __SONG__try "to_html" (
  let buf = Buffer.create 1024 in
  let () = to_html_print (Buffer.add_string buf) song  in
    Buffer.contents buf
)
