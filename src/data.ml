type alteration = | Sharp | Flat | Rien

module Chord = struct
  type t = {
    name : char ;
    alteration : alteration ;
    length  : int ;
    minor : bool ;
    mi7 : bool ;
    ma7 : bool ;
  }
end
  
module Section = struct
  type t = {
    name: string ;
    mesures_par_ligne: int list option ;
    chords : Chord.t list ;
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
  type position = {
    top : int ;
    left : int ;
    width : int ;
    height : int
  }
    
  type t = {
    name : string ;
    lyrics : position option ;
    grille : position option ;
    structure : position option ;
  }
end

module Song = struct 
  type t = {
    name : string ;
    format : string option ;
    sections : (string,Section.t) PMap.t ;
    structure : string list ;
    lyrics : Lyrics.t list ;
    outputs : Output.t list ;
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
