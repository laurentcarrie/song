open Printf
open ExtList
open Util
module D = Data

let (//) = Filename.concat


module PMap = struct 
  include PMap
  let find name m =
    try PMap.find name m with
      | Not_found -> __SONG__failwith("key '" ^ name ^ "' not found in map")
end

let log = Fcgi.log


let render_output world head print song output onoff sl = __SONG__try "render_html" (
    
  let pf fs = ksprintf print fs in
    
(*
    pf "
<script>
function left() {
console.log('left click') ;
} ;
function right() {
console.log('right click') ;
} ;
$(document).ready(function() {
  $('#left-click').click(left) ;
  $('#right-click').click(right) ;
}) ;
</script>
" ;

*)

    pf "
<script>
$(document).ready( function() {
  $('#reload').click(function() {
    console.log('reload') ;
    $.ajax({
       url:'/reload.songx',
       type : 'POST' ,
       success: function() { location.reload(); },
       error: function(data) { $('#error').text(data) ; }
    }) ;
  }) ;
}) ;

</script>
 "
;
    head() ;

    let filename = strip_root world song.D.Song.path in

    let () = match sl with
      | None -> log "sl is NONE" 
      | Some s -> log "sl is %s" s.D.Set_list.path
    in

    let () = match sl with
      | None -> log "sl is NONE" 
      | Some s -> log "sl is %s\nhas %d songs" s.D.Set_list.path (List.length s.D.Set_list.paths)
    in


    let search l rev =  match l with
      | None -> None
      | Some l -> (
	  let rec r l =
	    match l with
	      | [] -> None
	      | hd::tl -> (
		  log "compare %s and %s" hd song.D.Song.path ;
		  if hd = strip_root world song.D.Song.path then (
		    match tl with
		      | [] -> None
		      | hd::_ -> Some hd
		  ) else r tl
		)
	  in
	  let paths = if rev then List.rev l.D.Set_list.paths else l.D.Set_list.paths in
	    r paths
	)
    in

    let before = search sl true in
    let after  = search sl false in
      
    let () = match before with
      | None -> log "before is NONE" 
      | Some s -> log "sl is %s" s
    in

    let () = match after with
      | None -> log "after is NONE" 
      | Some s -> log "after is %s" s 
    in


      log "root = %s" world.D.World.root ;

      
  let () = match onoff with
      | On_line -> (
	  pf "<a href=\"/index.songx#song-%s\">index</a>" filename ;
	  pf "<a id='reload'>reload</a>"  ;
	  pf "<a href=\"/edit.songx?path=%s\">edit</a>" song.D.Song.path ;
	  List.iter ( fun o ->
	    pf "<a href=\"%s.html\">(%s)</a>" o.D.Output.filename o.D.Output.filename
	  ) song.D.Song.outputs ;
	  pf "<a href=\"/setlists.songx\">index</a>" ;
(*
	  pf "<a href=\"%s.midi\"><span class=\"index-title\">(midi)</span></a>" filename ;
	  pf "<a href=\"%s.wav\"><span class=\"index-title\">(wav)</span></a>" filename ;
	  pf "<a href=\"%s.mp3\"><span class=\"index-title\">(mp3)</span></a>" filename ;
	  pf "<a href=\"%s.pdf\"><span class=\"index-title\">(pdf)</span></a>" filename ;
*)
	  pf "<div id='error'></div>" ;
	  pf "<br/>" ;
	)
      | Off_line -> (
	  let path_to_index = 
	    let depth = List.length (path_to_list ("/"^filename)) in
	    let rec r path d =
	      if d=0 then path else r ("../"^path) (d-1)
	    in
	      r "index.html" depth
	  in
	    pf "<a href=\"%s#song-%s\">index</a>" path_to_index filename ;
	    pf "<br/>" ;
	)
  in

    (match sl with
      | None -> ()
      | Some sl -> pf "setlist : %s<br/>" sl.D.Set_list.title ;) ;
    
    pf "<a name='%s'/>" (strip_root world song.D.Song.path) ;

    pf " <div id=\"main-0\">

<div id=\"frame-title\">


<table class='title'>
<tr> " ;
    pf "<td class='left-click %s'>
<div id='left-click'> "
      ( match before with | None -> "inactive" | Some path -> "active" ) ; 
    ( match onoff with
      | On_line -> pf "<a href='/view.songx?path=%s&output=all&setlist=%s'>"
	  ( match before with | None -> "none" | Some path -> path ) 
	    ( match sl with | None -> "" | Some sl -> strip_root world sl.D.Set_list.path )
      | Off_line -> pf "<a href='#%s'>"
	  ( match before with | None -> "none" | Some path -> path ) 
    ) ;
    pf "
<img src='img/20140310105841100_easyicon_net_72.png'>
</a>
</img></div></td>" 
;
    pf "
<td>
<div class=\"title\" id='title'>
%s</div></td>\n" song.D.Song.title ;
    pf "<td class='right-click %s'>
<div id='right-click'> "
      ( match after with | None -> "inactive" | Some path -> "active" )  ;
    ( match onoff with
      | On_line -> pf "<a href='/view.songx?path=%s&output=all&setlist=%s'>"
	  ( match after with | None -> "none" | Some path -> path ) 
	    ( match sl with | None -> "" | Some sl -> strip_root world sl.D.Set_list.path )
      | Off_line ->  pf "<a href='#%s'>"
	  ( match after with | None -> "none" | Some path -> path ) 
    ) ;
    pf "
<img src='img/20140310105205472_easyicon_net_72.png'>
</a>
</img></div></td>" 
    ;
    pf "
</tr>
</table>
" ;
      pf "\
<div class=\"auteur\">%s</div>\n" song.D.Song.auteur ;

pf "
<!-- frame-title -->
</div> 
" ;
    let pp =
      List.iter ( fun s ->
		    match s with
		      | D.Output.L -> Lyrics.to_html_print print song ;
		      | D.Output.G -> Grille.to_html_print print song ;
		      | D.Output.S -> Structure.to_html_print print song ;
		      | D.Output.Lily -> Lilypond.to_html_print print onoff world song ;
		) 
    in

      pf "
<div id=\"main\">
<div id=\"col_1\">
" ;
      pp output.D.Output.col_1 ;
pf "
</div>
<div id=\"col_2\">
" ;
pp output.D.Output.col_2 ;
pf "
</div>
</div>
" ;
	  
)



(*
let render_html world print song  = __SONG__try ("render_html ") (
  match song.D.Song.outputs with
    | [] -> log "%s" "no output defined,... you will get no html file\n"
    | outputs -> (
	List.iter ( fun output ->
	  render_one_html world song output
	) outputs
      )
)
*)
