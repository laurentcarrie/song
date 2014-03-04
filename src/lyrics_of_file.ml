open Data
open Printf
open ExtString

let update_data song data = __SONG__try "read_file" (
  let data = Str.split (Str.regexp "\n") data in
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
	    | None,line -> let current = Some { Lyrics.name=line ; text="";index=None} in read acc current (linecount+1) tl
	    | Some current,"" -> read (current::acc) None (linecount+1) tl
	    | Some current,text ->let current = { current with Data.Lyrics.text=current.Data.Lyrics.text^line^"\n" } in read acc (Some current) (linecount+1) tl
	)
  in
  let data = read [] None 1 data in
    { song with Data.Song.lyrics = data }
)

let read_file song filename = __SONG__try "read_file" (
  let data = Std.input_file filename in
    update_data song data
)

let to_string song = __SONG__try "to_string" (
  let buf = Buffer.create 1024 in
  let pf fs = ksprintf ( fun s -> Buffer.add_string buf s ) fs in
    List.iter ( fun l ->
      pf "%s\n" l.Lyrics.name ;
      pf "%s\n\n" l.Lyrics.text 
    ) song.Song.lyrics ;
    Buffer.contents buf
)

let to_html_print print song = __SONG__try "to_html" (
  let pf fs = ksprintf print fs in
    pf "<h2>lyrics</h2>\n" ;
    pf "<ol class=\"lyrics-list\">\n" ;
    List.iter ( fun lyrics ->
      let text = lyrics.Lyrics.text in
      let text = Str.global_replace (Str.regexp "\\[\\(.*\\)\\]") (sprintf "<span class=\"lyrics-beat\">\\1</span>") text in 
      let text = Str.global_replace (Str.regexp "\n") "<br/>" text in
	pf "<li class=\"lyrics-list\"><span class=\"lyrics-section\">%s</span><br/>\n" lyrics.Lyrics.name ;
	pf "%s" text ;
	pf "</li>" ;
    ) song.Song.lyrics ;
    pf "</ol>\n" ;
)

let to_html song = __SONG__try "to_html" (
  let buf = Buffer.create 1024 in
  let () = to_html_print (Buffer.add_string buf) song in
    Buffer.contents buf
)

