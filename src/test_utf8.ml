open Printf
open ExtString

type status =
    | Normal
    | First
    | Second of char

let i_of_letter c = 
  match c with
    | '0' -> 0x00 
    | '1' -> 0x01 
    | '2' -> 0x02 
    | '3' -> 0x03 
    | '4' -> 0x04 
    | '5' -> 0x05 
    | '6' -> 0x06 
    | '7' -> 0x07 
    | '8' -> 0x08 
    | '9' -> 0x09 
    | 'A' -> 0x0A 
    | 'B' -> 0x0B 
    | 'C' -> 0x0C 
    | 'D' -> 0x0D 
    | 'E' -> 0x0E 
    | 'F' -> 0x0F
    | c    -> failwith ("bad letter : " ^ (Char.escaped c))


let from_url s = 
  let rec loop status chars buf count = match chars with
    | [] -> Buffer.contents buf
    | '+'::chars -> (
	Buffer.add_char buf ' ' ;
	loop status chars buf (count+1)
      )
    | c::chars -> ( match c with
	| '%' -> (
	    match status with
	      | Normal -> loop First chars buf (count+1)
	      | _ -> assert(false)
	  )
	| c  -> ( match status with
	    | Normal -> Buffer.add_char buf c; loop Normal chars buf (count+1)
	    | First  -> loop (Second c) chars buf (count+1)
	    | Second u1 -> (* loop (Third (u1,c)) chars buf (count+1) *)
		(
		  let uu = 16*(i_of_letter u1) + (i_of_letter c) in
		    Buffer.add_char buf (Char.chr uu) ; 
		    loop Normal chars buf (count+1)
		)
	  )
      )
  in
    loop Normal (String.explode s) (Buffer.create 512) 0
      
      
      


let f ?encoding (src : [`Channel of in_channel | `String of string]) =

  let rec loop d buf count = match Uutf.decode d with 
    | `Uchar u -> Uutf.Buffer.add_utf_8 buf u; loop d buf (count+1)
    | `End -> Buffer.contents buf 
    | `Malformed _ -> Uutf.Buffer.add_utf_8 buf Uutf.u_rep; loop d buf  (count+1)
    | `Await -> assert false
  in
  let nln = `Readline 0x000A in
    loop  (Uutf.decoder ~nln ?encoding src) (Buffer.create 512) 0
      
      
      

let _ = 
	let s = 
"XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX+%0AJe+vivais+dans+un'bulle%0AUne+vraie+tout+en+savon%0AQui+roulait+vers+l'cr%C3%A9puscule+%0Asans+Jamais+tourner+en+rond%0AComme+tu+m%E2`%99appelais+%C3%A0+toi%0AJe+suis+revenu+sur+terre%0AJ'oubliais+qu'sur+ton+toit%0AY'a+un+paratonnerre%0AQuand+tu+mas+dit+%E2`%9Cvolons%0APlus+haut+que+les+oiseaux%E2`%9C%0AJe+t'ai+offert+un+avion%0AY'en+avait+pas+d%E2`%99plus+beau%0AL%C3%A9ger+comme+une+feuille%0APli%C3%A9+selon+les+traits%0AD%E2`%99ailleurs+c%E2`%99%C3%A9tait+une+feuille+/+oh+non%0AS'te+pla%C3%AEt+range+ce+briquet%C2%A0%E2`%A6%0A+%0AQui+br%C3%BBle+tout%0A%E2`%A6%0AEt+moi+j%E2`%99oublie+tout%0A%E2`%A6%0A+%0ATu+r%C3%AAvais+d%E2`%99une+maison%0AJe+t%E2`%99ai+construit+un+palais%0AAvec+vue+sur+l%E2`%99horizon%0AJ%E2`%99%C3%A9tais+s%C3%BBr+qu%E2`%99%C3%A7a+t%E2`%99plairait%0AT%E2`%99as+dit+%E2`%9Cles+ch%C3%A2teaux+de+sable%0ACa+fait+vraiment+trop+kitsch%0AOublie,+t%E2`%99es+pas+capable%E2`%9C+/+et%0AT%E2`%99es+retourn%C3%A9e+au+Palm+Beach%0ATous+ces+colliers+de+nouilles%0APass%C3%A9s+%C3%A0+la+casserole%0A+%0ADignes+d%E2`%99un+roi+du+p%C3%A9trole%0APlut%C3%B4t+que+deux-trois+gosses+/+qui%0AFlingueraient+tes+jolis+lolos%0AAdopte+donc+un+molosse%0AC%E2`%99est+propre+et+%C3%A7a+sait+faire+le+beau%0A+%0ATu+br%C3%BBles+tout%0A%E2`%A6%0AEt+moi+j%E2`%99oublie+tout%0A%E2`%A6%0A+%0AJe+vivais+dans+un%E2`%99bulle%0AUne+vraie+tout+en+savon%0AQui+roulait+vers+l%E2`%99cr%C3%A9puscule+/+sans%0AJamais+tourner+en+rond%0AComme+tu+m%E2`%99appelais+%C3%A0+toi%0AJe+suis+revenu+sur+terre%0AJ%E2`%99oubliais+qu%E2`%99dans+ta+t%C3%AAte%0AC%E2`%99est+plein+d%E2`%99paratonnerres%0ALa+bulle+a+%C3%A9clat%C3%A9%0AIl+a+plu+sur+l%E2`%99palais%0AL%E2`%99avion+s%E2`%99est+envol%C3%A9%0AJ%E2`%99%C3%A9tais+seul+au+manche+%C3%A0+balai%0ANe+cherche+plus+tes+r%C3%AAves%0AJe+les+ai+emmen%C3%A9s%0ADe+ton+nom+sur+la+gr%C3%A8ve%0ALes+vagues+n%E2`%99ont+rien+laiss%C3%A9%0A+%0ABr%C3%BBle+tout%0ACe+que+j%E2`%99ai+pu+t%E2`%99%C3%A9crire%0AOublie+tout%0ACe+que+j%E2`%99ai+pu+te+dire%0ABr%C3%BBle+tout%0A+%0A" in

	let s = from_url s in
	  (* printf "%s\n" s ; *)
	  
	let s = f (`String s) in
	  (* let s  = String.implode (List.map Char.chr s) in *)
	  printf "%s\n" s
	    
