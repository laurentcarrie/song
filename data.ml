module Chord = struct
  type t = {
    name : char ;
    length  : int ;
    minor : bool ;
    mi7 : bool ;
    ma7 : bool ;
  }
end
  
module Section = struct
  type t = {
    name: string ;
    chords : Chord.t list ;
  }
end

module Song = struct 
  type t = {
    name : string ;
    sections : (string,Section.t) PMap.t ;
    structure : string list ;
    lyrics : (string (* nom de la section *) * ((int option*string) list)) list ;
  }
end

module View = struct
  type t = {
    filename : string
  }
end

