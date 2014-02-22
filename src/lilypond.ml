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

    pf "\\version \"2.12.3\"
\\header {hello world} " ;


    pf "drl = \\drummode { " ; 
    (* 1 mesure vide au début *)
    pf "bd4 sn4 bd4 sn4" ;
    pf "bd4 sn4 bd4 sn4" ;
    pf "bd4 sn4 bd4 sn4" ;
    pf "bd4 sn4 bd4 sn4" ;
    List.iter ( fun s ->
      let section = PMap.find s.Structure.section_name song.Song.sections in
	List.iter ( fun c ->
	  match c with
	    | Section.NL -> ()
	    | Section.C c ->  (
		match c.Chord.length with
		  | 2 -> pf  " bd4 sn4 "  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | 4 -> pf  " bd4 sn4 bd4 sn4"  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | i -> __SONG__failwith ("length not managed : " ^ (string_of_int i))
	      )
	) section.Section.chords ;
    ) song.Song.structure ;
    pf " } " ;


    pf "drh = \\drummode { " ; 
    (* 1 mesure vide au début *)
    List.iter ( fun s ->
      let section = PMap.find s.Structure.section_name song.Song.sections in
	List.iter ( fun c ->
	  match c with
	    | Section.NL -> ()
	    | Section.C c ->  (
		match c.Chord.length with
		  | 2 -> pf  "  ss8 ss8 ss8 ss8 "  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | 4 -> pf  " ss8 ss8 ss8 ss8 ss8 ss8 ss8 ss8"  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | i -> __SONG__failwith ("length not managed : " ^ (string_of_int i))
	      )
	) section.Section.chords ;
    ) song.Song.structure ;

    pf " } 
%% timb = \\drummode { timh4 ssh timl8 ssh r timh r4 ssh8 timl r4 cb8 cb }

melody = \\relative c' {
  \\clef treble
  \\key c \\major
  \\time 4/4

%%  f4 e8[ c] d4 g
%%  a2 ~ a
}

harmonies = \\chordmode { " ;
    (* 1 mesure vide au début *)
    pf "r1 " ;
    pf "r1 " ;
    pf "r1 " ;
    pf "r1 " ;

    List.iter ( fun s ->
      let section = PMap.find s.Structure.section_name song.Song.sections in
	List.iter ( fun c ->
	  match c with
	    | Section.NL -> ()
	    | Section.C c ->  (
		let rec p count =
		  if count=0 then ()
		  else (
		    pf "%s" (Note.lilypond_name c.Chord.note 4) ;
		    p (count-1)
		  )
		in
		  p (c.Chord.length)
	      )
	) section.Section.chords ;
    ) song.Song.structure ;

    pf "
%%  c4:m f:min7 g:maj c:aug
%%  d2:dim b:sus
}

\\score {
  << " ;
    pf "\\tempo 4 = %d " song.Song.tempo  ;

    pf "

    \\new DrumStaff <<
      \\set Staff.instrumentName = #\"drums\"
      \\new DrumVoice { \\stemUp \\drh }
      \\new DrumVoice { \\stemDown \\drl }
    >>

    \\new ChordNames {
    \\set chordChanges = ##t
      \\harmonies
    }


    \\new Staff \\melody


  >>
  \\layout { }
  \\midi {
  }
} " 
 ;

    close_out fout ;
  (
    Array.iter ( fun s ->
      log "env :%s" s
    ) (Unix.environment())
  ) ;

    let lilypond = __SONG__try "LILYPOND" (Unix.getenv "LILYPOND") in
    let timidity = __SONG__try "TIMIDITY" (Unix.getenv "TIMIDITY") in
    let pacpl = __SONG__try "PACPL" (Unix.getenv "PACPL") in

    let commands = [
      sprintf "%s --relocate --verbose --output \"%s\" \"%s.ly\" &>> /var/log/lighttpd/lilypond.log " 
	lilypond (output_dir//base_filename) (output_dir//base_filename) ;
      sprintf "%s -Ow \"%s.midi\"" timidity (output_dir//base_filename)  ;
      sprintf "%s --to mp3 \"%s.wav\"" pacpl (output_dir//base_filename) 
    ] in

      List.iter ( fun command ->
		    let () = __SONG__try command ( 
		      log "running %s" command ;
		      match Unix.system command with
			| Unix.WEXITED 0 -> ()
			| Unix.WEXITED i -> __SONG__failwith ("exited with code " ^ (string_of_int i))
			| Unix.WSTOPPED i -> __SONG__failwith ("stopped with code " ^ (string_of_int i))
			| Unix.WSIGNALED i -> __SONG__failwith ("signaled with code " ^ (string_of_int i))
		    ) in
		      ()
		) commands 
)

