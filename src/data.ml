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
  let text_name t duree = sprintf "%c%s%s%s\\%d" 
    t.letter
    ( if t.is_sharp then "#" else "")
    ( if t.is_flat then "b" else "")
    ( if t.is_m then "m" else "")
    duree
  let html_name t duree = sprintf "%c%s%s%s%s" 
    t.letter
    ( if t.is_sharp then "<sup>&#9839</sup>" else "")
    ( if t.is_flat then "<sup>&#x266d</sup>" else "")
    ( if t.is_m then "m" else "")
    (* ( sprintf "<sub>%d</sub>" duree ) *) ""
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
      | G of Chord.t list
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
    | Lily (* lilypond *)
  type t = {
    filename : string ;
    col_1 : w list ;
    col_2 : w list ;
    width : int ;
  }
end

module Lilypond = struct
  type s =
      | Unknown
      | Ok of string (* filename *)
      | Error of string (* error msg *)
  type t = {
    data : string ;
    status : s ;
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
    format : string option ;
    sections : Section.t list ;
    structure : Structure.t list ;
    lyrics : Lyrics.t list ;
    outputs : Output.t list ;
    tempo : int ;
    path : string ;
    lilyponds : Lilypond.t list ;
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
    errors : string list ;
    root : string ;        (* la racine du chemin de recherche des chansons, sur le disque *)
    output_root : string ; (* la ou sont generes les fichiers *)
    doc_root : string ;    (* le doc_root du server web *)
    url_root : string ;    (* le chemin relatif pour l'url *)
    root_path : string ;
  }
end

let (r_world:World.t option ref) = ref None
  
let world () = match !r_world with
  | Some w -> w
  | None   -> __SONG__failwith "World non défini"
    

let update_world world = 
  r_world := Some world

let update_world_songs world songs =
  let world = { world with World.songs = songs } in
    r_world := Some world ;
    world
      

let update_world_errors world errors =
  let world = { world with World.errors = errors } in
    r_world := Some world ;
    world

let update_song world song = 
  let songs = List.fold_left ( fun acc s ->
    let s = 
      if s.Song.path = song.Song.path then song else s in
      s::acc
  ) [] world.World.songs in
  let world =  { world with World.songs = songs } in
  let () = update_world world in
    world
      
	


let add_world_song song =
  let world = match !r_world with
    | Some world ->  { world with World.songs = song::world.World.songs }
    | None   -> __SONG__failwith "World non défini"
  in
    r_world := Some world

