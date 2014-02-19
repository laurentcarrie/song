open Data
open Printf
open ExtList
open Util

let (//) = Filename.concat

let log = Fcgi.log

let render song  output_dir = __SONG__try "write lilypond" (
  let base_filename =  sprintf "%s-%s" (Str.global_replace (Str.regexp " ") "_" song.Song.auteur) (Str.global_replace (Str.regexp " ") "_" song.Song.title) in
  let filename = output_dir // ( base_filename ^ ".ly") in
  let () = log "writing %s...\n" filename in

  let fout =  open_out_bin filename in
    
  let pf fs = ksprintf (fun s -> fprintf fout "%s\n" s) fs in
    pf "
\\version \"2.12.3\"
\\header {}

drh = \\drummode { cymc4.^\"crash\" hhc16^\"h.h.\" hh hhc8 hho hhc8 hh16 hh hhc4 r4 r2 }
drl = \\drummode { bd4 sn8 bd bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh }
timb = \\drummode { timh4 ssh timl8 ssh r timh r4 ssh8 timl r4 cb8 cb }

\\score {
  <<
    \\tempo 4 = 120

    \\new DrumStaff \\with {
      drumStyleTable = #timbales-style
      \\override StaffSymbol #'line-count = #2
      \\override BarLine #'bar-extent = #'(-1 . 1)
    } <<
      \\set Staff.instrumentName = #\"timbales\"
      \\timb
    >>
    \\new DrumStaff <<
      \\set Staff.instrumentName = #\"drums\"
      \\new DrumVoice { \\stemUp \\drh }
      \\new DrumVoice { \\stemDown \\drl }
    >>
  >>
  \\layout { }
  \\midi {
  }
} " ;

    close_out fout ;
    let command = sprintf "lilypond --relocate --verbose --output \"%s\" \"%s.ly\" 2> /var/log/lighttpd/lilypond.log " 
      (output_dir//base_filename) (output_dir//base_filename) in 

    let () = __SONG__try command ( 
      match Unix.system command with
	| Unix.WEXITED 0 -> ()
	| Unix.WEXITED i -> __SONG__failwith ("exited with code " ^ (string_of_int i))
	| Unix.WSTOPPED i -> __SONG__failwith ("stopped with code " ^ (string_of_int i))
	| Unix.WSIGNALED i -> __SONG__failwith ("signaled with code " ^ (string_of_int i))
    ) in
      ()
)

