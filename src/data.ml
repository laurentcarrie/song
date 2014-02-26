open Printf

type alteration = | Sharp | Flat | Rien



module Note = struct 
  type t = {
    letter : char ;
    is_7   : bool ;
    is_7M : bool ;
    is_m  : bool ;
    is_flat : bool ;
    is_sharp : bool ;
  }
  let name t = sprintf "%c%s%s%s" 
    t.letter
    ( if t.is_m then "m" else "")
    ( if t.is_sharp then "#" else "")
    ( if t.is_flat then "b" else "")
  let html_name t duree = sprintf "%c%s%s%s%s" 
    t.letter
    ( if t.is_m then "m" else "")
    ( if t.is_sharp then "<sup>&#9839</sup>" else "")
    ( if t.is_flat then "<sup>&#x266d</sup>" else "")
    ( sprintf "<sub>%d</sub>" duree )
  let lilypond_name t duree = sprintf "%c%s%s%d%s" 
    (Char.lowercase t.letter)
    ( if t.is_sharp then "is" else "")
    ( if t.is_flat then "es" else "")
    duree 
    ( if t.is_m then ":m" else "")
end
  


module Chord = struct
  type t = {
    note : Note.t ;
    length  : int ;
  }
end
  
module Section = struct
  type c = 
      | C of Chord.t
      | NL
  type t = {
    name: string ;
    chords : c list ;
    signature : int ;
  }
end

module Lyrics = struct
  type t = {
    name : string ;
    text : string ;
    index : int option ;
  }
end

module Output = struct
  type w =
    | L (* lyrics *)
    | G (* grille *)
    | S (* structure *)
  type t = {
    filename : string ;
    col_1 : w list ;
    col_2 : w list ;
    width : int ;
  }
end

module Structure = struct
  type t = {
    section_name : string ;
    comment : string ;
  }
end

module Song = struct 
  type t = {
    title : string ;
    auteur : string ;
    filename : string ;
    format : string option ;
    sections : (string,Section.t) PMap.t ;
    structure : Structure.t list ;
    lyrics : Lyrics.t list ;
    outputs : Output.t list ;
    tempo : int ;
    (* lyrics : (string (* nom de la section *) * ((int option*string) list)) list ; *)
  }

end

module View = struct
  type t = {
    filename : string
  }
end

let line_number = ref 1

type context = | IN_TEXT | OUT_TEXT
let context = ref OUT_TEXT


  
module World = struct
  type t = {
    songs : Song.t list ;
  }
end

let world = ref { World.songs = [] }
