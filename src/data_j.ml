open Printf
module Bu = Json_type.Build
module Br = Json_type.Browse

let string_field table name = __SONG__try ("string_field " ^ name) (
  Br.string (Br.field table name)
)
  
let int_field table name = __SONG__try ("int_field " ^ name) (
  Br.int (Br.field table name)
)

let bool_field table name = __SONG__try ("bool_field " ^ name) (
  Br.bool (Br.field table name)
)

type alteration = | Sharp | Flat | Rien



module Note = struct 
  include Data.Note
  let of_json j = __SONG__try "of_json" (
    let j = Br.objekt j in
    let table = Br.make_table j in
      {
	letter = String.get (string_field table "letter") 0 ;
	is_7 = bool_field table "is_7" ;
	is_7M = bool_field table "is_7M" ;
	is_m = bool_field table "is_m" ;
	is_flat = bool_field table "is_flat" ;
	is_sharp = bool_field table "is_sharp" ;
      }
  )
  let to_json t = __SONG__try "to_json" (
    Bu.objekt [
      "letter",Bu.string (Char.escaped t.letter) ;
      "is_7",Bu.bool t.is_7 ;
      "is_7M",Bu.bool t.is_7M ;
      "is_m",Bu.bool t.is_m ;
      "is_flat",Bu.bool t.is_flat ;
      "is_sharp",Bu.bool t.is_sharp ;
    ]
  )
end
  


module Chord = struct
  include Data.Chord
  let of_json j = __SONG__try "of_json" (
    let j = Br.objekt j in
    let table = Br.make_table j in
      {
	note = Note.of_json (Br.field table "note") ;
	length = int_field table "length" ;
      }
  )
  let to_json t = __SONG__try "to_json" (
    Bu.objekt [
      "note",Note.to_json t.note ;
      "length",Bu.int t.length ;
    ]
  )
end
  
module Section = struct
  include Data.Section
  let c_of_json j = __SONG__try "of_json" (
    let table = Br.make_table(Br.objekt j) in
    match Br.optfield table "NL",Br.optfield table "chord" with
      | None,None -> __SONG__failwith "internal error"
      | Some _,Some _ -> __SONG__failwith "internal error"
      | Some _,None -> NL
      | None, Some j -> C (Chord.of_json j)
  )
    
  let json_of_c c =
    match c with
      | NL -> Bu.string "NL" ;
      | C c -> Chord.to_json c
	  
  let of_json j = __SONG__try "of_json" (
    let j = Br.objekt j in
    let table = Br.make_table j in
      {
	name = string_field table "name" ;
	chords = Br.list c_of_json (Br.field table "chords") ;
	signature = int_field table "signature" ;
      }
  )

  let to_json t = __SONG__try "to_json" (
    Bu.objekt [
      "name",Bu.string t.name ;
      "chords",Bu.list json_of_c t.chords ;
      "signature",Bu.int t.signature ;
    ]
  )
end

module Lyrics = struct
  include Data.Lyrics
  let of_json j = __SONG__try "of_json" (
    let j = Br.objekt j in
    let table = Br.make_table j in
      {
	name = string_field table "name" ;
	text = string_field table "text" ;
	index = None ;
      }
  )
  let to_json t = __SONG__try "to_json" (
    Bu.objekt [
      "name",Bu.string t.name ;
      "text",Bu.string t.text ;
    ]
  )
end

module Output = struct
  include Data.Output
  let col_of_json j = __SONG__try "of_json" (
    match Br.string j with
      | "L" -> L
      | "G" -> G
      | "S" -> S
      | s -> __SONG__failwith s
  )
  let json_of_col c = 
    Bu.string (match c with
      | L -> "L"
      | G -> "G"
      | S -> "S")


  let of_json j = __SONG__try "of_json" (
    let j = Br.objekt j in
    let table = Br.make_table j in
      {
	filename = string_field table "filename" ;
	col_1 = Br.list col_of_json (Br.field table "col_1") ;
	col_2 = Br.list col_of_json (Br.field table "col_2") ;
	width = int_field table "width"
      }
  )
  let to_json t = __SONG__try "to_json" (
    Bu.objekt [
      "filename",Bu.string t.filename ;
      "col_1",Bu.list json_of_col t.col_1 ;
      "col_2",Bu.list json_of_col t.col_2 ;
      "width",Bu.int t.width ;
    ]
  )

end

module Structure = struct
  include Data.Structure
  let of_json j = __SONG__try "of_json" (
    let j = Br.objekt j in
    let table = Br.make_table j in
      {
	section_name = string_field table "section_name" ;
	comment = string_field table "comment" ;
      }
  )
  let to_json t = __SONG__try "to_json" (
    Bu.objekt [
      "section_name",Bu.string t.section_name ;
      "comment",Bu.string t.comment ;
    ]
  )
end

module Song = struct 
  include Data.Song
  let of_json j = __SONG__try "song_of_json" (
    let j = Br.objekt j in
    let table = Br.make_table j in
      {
	title = Br.string (Br.field table "title") ;
	auteur = Br.string (Br.field table "auteur") ;
	filename = Br.string (Br.field table "filename") ;
	format = None ;
	sections = List.fold_left ( fun acc s -> PMap.add s.Section.name s acc )
	  (PMap.create String.compare) ( Br.list Section.of_json (Br.field table "sections" ))  ;
	structure = Br.list Structure.of_json (Br.field table "structure") ;
	lyrics = Br.list Lyrics.of_json (Br.field table "lyrics") ;
	outputs = Br.list Output.of_json (Br.field table "outputs") ;
	tempo = Br.int (Br.field table "tempo") ;
      }
  )

  let to_json t = __SONG__try "to_json" (
    Bu.objekt [
      "title",Bu.string t.title ;
      "auteur",Bu.string t.auteur ;
      "filename",Bu.string t.filename ;
      "sections",Bu.list Section.to_json (PMap.foldi ( fun k v acc -> v::acc ) t.sections []) ;
      "structure",Bu.list Structure.to_json t.structure ;
      "lyrics",Bu.list Lyrics.to_json t.lyrics ;
      "outputs",Bu.list Output.to_json t.outputs ;
      "tempo",Bu.int t.tempo ;
    ]
  )


end

  
