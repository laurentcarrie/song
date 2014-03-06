open Printf
open ExtList
open Util
module D = Data

let (//) = Filename.concat

let log = Fcgi.log

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
	    | D.Section.C c ->  (
		match c.D.Chord.length with
		  | 2 -> pf  " bd4 sn4 "  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | 4 -> pf  " bd4 sn4 bd4 sn4"  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | i -> __SONG__failwith ("length not managed : " ^ (string_of_int i))
	      )
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
	    | D.Section.C c ->  (
		match c.D.Chord.length with
		  | 2 -> pf  "  ss8 ss8 ss8 ss8 "  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | 4 -> pf  " ss8 ss8 ss8 ss8 ss8 ss8 ss8 ss8"  ; (* bd4 << bd ss >>  bd8 tommh tommh bd toml toml bd tomfh16 tomfh *)
		  | i -> __SONG__failwith ("length not managed : " ^ (string_of_int i))
	      )
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
  let data = Str.split (Str.regexp "========\n") data in
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
	    | None,line -> let current = Some { D.Lilypond.name="XXXXXXXXXX" ; data=line;} in read acc current (linecount+1) tl
	    | Some current,"" -> read (current::acc) None (linecount+1) tl
	    | Some current,text ->let current = { current with D.Lilypond.data=current.D.Lilypond.data^line^"\n" } in read acc (Some current) (linecount+1) tl
	)
  in
  let data = read [] None 1 data in
    { song with D.Song.lilyponds = data }
)

let read_file song filename = __SONG__try "read_file" (
  let data = Std.input_file filename in
    update_data song data
)

let to_print print song = __SONG__try "to_string" (
  let pf fs = ksprintf print fs in
    List.iter ( fun l ->
      pf "=== BEGIN LILYPONDS ===" ;
      pf "%s\n\n" l.D.Lilypond.data ; 
      pf "=== END LILYPONDS ===" ;
    ) song.D.Song.lilyponds
)

let to_string song = __SONG__try "to_string" (
  let buf = Buffer.create 1024 in
  let print = Buffer.add_string buf in
    to_print print song ;
    Buffer.contents buf
)


let to_png world song =  __SONG__try "to_png" (
  let lilypond = try Unix.getenv "LILYPOND" with | Not_found -> __SONG__failwith "LILYPOND env variable not set" in
  let () = log "to_png lilypond %d sections" (List.length song.D.Song.lilyponds) in
    List.iteri ( fun index l ->
      let filename = strip_root world song.D.Song.path in
      let digest_filename = sprintf "%s/tmp/%s-%d.digest" world.D.World.doc_root filename index in
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
      let () = log "lilypond do_it = %b ; %d" do_it index in
	if do_it then (
	  let () = 
	    let filename = sprintf "%s/tmp/%s-%d.ly" 	world.D.World.doc_root filename index in
	    let () = log "lilypond : %s" filename in
	      Std.output_file ~filename ~text:l.D.Lilypond.data 
	  in
	  let command = sprintf "%s --verbose --png --output \"%s/tmp/%s-%d\" \"%s/tmp/%s-%d.ly\" &> /var/log/lighttpd/lilypond-%s-%d.log " 
	    lilypond 
	    world.D.World.doc_root filename index 
	    world.D.World.doc_root filename index 
	    filename index 
	  in
	  let () = __SONG__try command ( 
	    log "running %s" command ;
	    match Unix.system command with
	      | Unix.WEXITED 0 -> log "command ok" ; ()
	      | Unix.WEXITED i -> __SONG__failwith ("exited with code " ^ (string_of_int i))
	      | Unix.WSTOPPED i -> __SONG__failwith ("stopped with code " ^ (string_of_int i))
	      | Unix.WSIGNALED i -> __SONG__failwith ("signaled with code " ^ (string_of_int i))
	  ) in
	  let () = Std.output_file ~filename:digest_filename ~text:lilypond_hex in
	    ()
	) else (
	)
    ) song.D.Song.lilyponds

)

let to_html_print print onoff world song = __SONG__try "to_html" (
  let () = log "to hml print" in
  let () = to_png world song in
  let pf fs = ksprintf print fs in
    pf "<h2>lilypond</h2>\n" ;
    pf "<ol class=\"lilypond-list\">\n" ;
    let filename = strip_root world song.D.Song.path in
    List.iteri ( fun index lily ->
      (match onoff with
	| On_line -> pf "<img src='/tmp/%s-%d.png'></img>"  filename index
	| Off_line -> pf "<img src='%s-%d.png'/>" filename index
      ) ;
      pf "</li>" ;
    ) song.D.Song.lilyponds ;
      pf "</ol>\n" ;
)

let to_html world onoff song = __SONG__try "to_html" (
  let buf = Buffer.create 1024 in
  let () = to_html_print (Buffer.add_string buf) onoff world song in
    Buffer.contents buf
)

