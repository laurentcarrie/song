open Printf
open ExtList
open Util
module D = Data

let (//) = Filename.concat

let log = Fcgi.log

let r_count = ref 0
let count () =
  incr r_count ;
  !r_count
    
let render world song   = __SONG__try "write lilypond" (
  let filename = world.D.World.output_root // ((Filename.basename song.D.Song.path) ^ ".ly") in
  let () = log "writing %s..." filename in

  let fout =  open_out_bin filename in
    
  let pf fs = ksprintf (fun s -> fprintf fout "%s\n" s) fs in

    pf "\\version \"2.12.3\"
%%\\header {hello world} " ;


    pf "drl = \\drummode { " ; 
    (* 1 mesure vide au début *)
    pf "bd4 sn4 bd4 sn4" ;
    pf "bd4 sn4 bd4 sn4" ;
    pf "bd4 sn4 bd4 sn4" ;
    pf "bd4 sn4 bd4 sn4" ;
    List.iter ( fun s ->
      let section = List.find ( fun current -> current.D.Section.name = s.D.Structure.section_name) song.D.Song.sections in
	List.iter ( fun c ->
	  match c with
	    | D.Section.NL -> ()
(*
	    | D.Section.C c ->  (
		match c.D.Chord.length with
		  | 2 -> pf  " bd4 sn4 "  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | 4 -> pf  " bd4 sn4 bd4 sn4"  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | i -> __SONG__failwith ("length not managed : " ^ (string_of_int i))
	      )
*)
	    | D.Section.G g -> __SONG__NOT_IMPLEMENTED__
	) section.D.Section.chords ;
    ) song.D.Song.structure ;
    pf " } " ;


    pf "drh = \\drummode { " ; 
    (* 1 mesure vide au début *)
    List.iter ( fun s ->
      let section = List.find ( fun current -> current.D.Section.name = s.D.Structure.section_name) song.D.Song.sections in
	List.iter ( fun c ->
	  match c with
	    | D.Section.NL -> ()
(*
	    | D.Section.C c ->  (
		match c.D.Chord.length with
		  | 2 -> pf  "  ss8 ss8 ss8 ss8 "  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | 4 -> pf  " ss8 ss8 ss8 ss8 ss8 ss8 ss8 ss8"  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | i -> __SONG__failwith ("length not managed : " ^ (string_of_int i))
	      )
*)
	    | D.Section.G g -> __SONG__NOT_IMPLEMENTED__
	) section.D.Section.chords ;
    ) song.D.Song.structure ;

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
      let section = List.find ( fun current -> current.D.Section.name = s.D.Structure.section_name) song.D.Song.sections in
	List.iter ( fun c ->
	  match c with
	    | D.Section.NL -> ()
(*
	    | D.Section.C c ->  (
		let rec p count =
		  if count=0 then ()
		  else (
		    pf "%s" (D.Note.lilypond_name c.D.Chord.note 4) ;
		    p (count-1)
		  )
		in
		  p (c.D.Chord.length)
	      )
*)
	    | D.Section.G _ -> __SONG__NOT_IMPLEMENTED__
	) section.D.Section.chords ;
    ) song.D.Song.structure ;

    pf "
%%  c4:m f:min7 g:maj c:aug
%%  d2:dim b:sus
}

\\score {
  << " ;
    pf "\\tempo 4 = %d " song.D.Song.tempo  ;

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
    (*
      Array.iter ( fun s ->
      log "env :%s" s
      ) (Unix.environment())
    *) ;

      
    let commands = [] in
    let filename = Filename.basename song.D.Song.path in
    let commands = 
      try
	let pacpl = Unix.getenv "PACPL" in
	  (sprintf "%s --to mp3 \"%s.wav\"" pacpl (world.D.World.output_root // filename)) :: commands
      with
	| Not_found -> (
	    log "PACPL not found" ; commands
	  )
    in
      
    let commands = 
      try
	let timidity = Unix.getenv "TIMIDITY" in
	  (sprintf "%s -Ow \"%s.midi\"" timidity (world.D.World.output_root // filename)) :: commands
      with
	| Not_found -> (
	    log "TIMIDITY not found" ; commands
	  )
    in
	    
    let commands =
      try 
	let lilypond = Unix.getenv "LILYPOND" in
	  sprintf "%s --relocate --verbose --output \"%s\" \"%s.ly\" &> /var/log/lighttpd/lilypond-%s.log " 
	    lilypond (world.D.World.output_root//filename) (world.D.World.output_root//filename) filename :: commands
      with
	| Not_found -> (
	    log "TIMIDITY not found" ; commands
	  )
    in
      
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




let update_data song data = __SONG__try "read_file" (
  let data = Str.split (Str.regexp (Str.quote "=== END ===\n")) data in
    log "split : %d" (List.length data) ;
    List.iteri ( fun index s ->
      log "lilypond split %d %s" index s
    ) data ;
    let data = List.map ( fun s -> { D.Lilypond.data=s ; status = D.Lilypond.Unknown ; } ) data in
    { song with D.Song.lilyponds = data }
)

let read_file song filename = __SONG__try "read_file" (
  let data = Std.input_file filename in
    update_data song data
)

let to_print print song = __SONG__try "to_string" (
  let pf fs = ksprintf print fs in
    List.iter ( fun l ->
      pf "%s\n\n=== END ===\n\n" l.D.Lilypond.data ; 
    ) song.D.Song.lilyponds
)

let to_string song = __SONG__try "to_string" (
  let buf = Buffer.create 1024 in
  let print = Buffer.add_string buf in
    to_print print song ;
    Buffer.contents buf
)


let generate_png world song =  __SONG__try "generate_png" (
  let lilypond = try Unix.getenv "LILYPOND" with | Not_found -> __SONG__failwith "LILYPOND env variable not set" in
  let () = log "genearte_png lilypond %d sections" (List.length song.D.Song.lilyponds) in
  let (lilyponds:D.Lilypond.t list) = List.fold_left ( fun acc l ->
    let filename = strip_root world song.D.Song.path in
    let png_filename = match l.D.Lilypond.status with
      | D.Lilypond.Ok filename -> filename
      | D.Lilypond.Error _ 
      | D.Lilypond.Unknown -> sprintf "%d.png" (count ())
    in
    let basename = 
      let basename = Filename.basename png_filename in
	Filename.chop_extension basename
    in
    let () = log "basename = %s" basename in
    let digest_filename = sprintf "%s/tmp/%s.digest" world.D.World.doc_root basename in
    let digest_hex = if Sys.file_exists digest_filename then Some (Std.input_file digest_filename) 
      else  (
	log "digest_filename does not exist" ;
	None )
    in
    let lilypond_hex = Digest.to_hex (Digest.string l.D.Lilypond.data) in
    let do_it = match digest_hex with
      | None -> true
      | Some d -> log "digest compare '%s' '%s'" d lilypond_hex ; d <> lilypond_hex
    in
    let () = log "lilypond do_it = %b ; %s" do_it png_filename in
    let status = 
      if do_it then (
	let ly_filename = sprintf "%s/tmp/%s-%s.ly" 	world.D.World.doc_root filename basename in
	let () = log "lilypond : %s" ly_filename in
	let () = Std.output_file ~filename:ly_filename ~text:l.D.Lilypond.data in
	let command = sprintf "%s --verbose --png --output \"%s/tmp/%s\" \"%s\" &> /var/log/lighttpd/lilypond-%s.log " 
	  lilypond 
	  world.D.World.doc_root basename 
	  ly_filename
	  basename
	in
	let status = 
	  log "running %s" command ;
	  let msg () = __SONG__try "read error file" ( 
	    Std.input_file (sprintf "/var/log/lighttpd/lilypond-%s.log"  basename )) 
	  in
	    match Unix.system command with
	      | Unix.WEXITED 0 ->   log "command ok" ; D.Lilypond.Ok png_filename
	      | Unix.WEXITED i ->   D.Lilypond.Error (sprintf "exited with code %d\n%s"  i (msg()))
	      | Unix.WSTOPPED i ->  D.Lilypond.Error (sprintf "stopped with code %d\n%s"  i (msg()))
	      | Unix.WSIGNALED i -> D.Lilypond.Error (sprintf "signaled with code %d\n%s"  i (msg()))
	in
	let () = Std.output_file ~filename:digest_filename ~text:lilypond_hex in
	  status
      ) else (
	match l.D.Lilypond.status with
	  | D.Lilypond.Ok _ 
	  | D.Lilypond.Unknown -> D.Lilypond.Ok png_filename
	  | D.Lilypond.Error e -> D.Lilypond.Error e
      )
    in
      { l with D.Lilypond.status = status }::acc
  ) [] song.D.Song.lilyponds in
  let song = { song with D.Song.lilyponds = List.rev lilyponds } in
    song
)

let to_html_print print onoff world song = __SONG__try "to_html" (
  let () = log "to hml print" in
  let pf fs = ksprintf print fs in
    pf "<h2>lilypond</h2>\n" ;
    pf "<ol class=\"lilypond-list\">\n" ;
    List.iteri ( fun index lily ->
      (match onoff with
	| On_line -> (
	    match lily.D.Lilypond.status with 
	      | D.Lilypond.Ok filename -> pf "<img src='/tmp/%s'></img>"   filename
	      | D.Lilypond.Unknown  -> pf "<p>UNKNOWN</p>"
	      | D.Lilypond.Error msg -> pf "<p>%s</p>" msg
	  )
	| Off_line -> (
	    match lily.D.Lilypond.status with 
	      | D.Lilypond.Ok filename -> pf "<img src='%s'></img>"  filename 
	      | D.Lilypond.Unknown  -> pf "<p>UNKNOWN</p>"
	      | D.Lilypond.Error msg -> pf "<p>%s</p>" msg
	  )
      ) ;
      pf "</li>" ;
    ) song.D.Song.lilyponds ;
    pf "</ol>\n"
)
  
let to_html world onoff song = __SONG__try "to_html" (
  let buf = Buffer.create 1024 in
  let () = to_html_print (Buffer.add_string buf) onoff world song in
    Buffer.contents buf 
)

