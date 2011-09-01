type chord_t = {
  cname : char ;
  clength  : int ;
  minor : bool ;
  mi7 : bool ;
  ma7 : bool ;
}

type section_t = {
  sname: string ;
  chords : chord_t list ;
}

type song_t = {
  name : string ;
  sections : (string,section_t) PMap.t ;
  structure : string list ;
  lyrics : (string (* nom de la section *) * ((int option*string) list)) list ;
}

type view_t = {
  filename : string
}

