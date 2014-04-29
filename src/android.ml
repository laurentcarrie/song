let (data:(string,string list)PMap.t ref)  = ref (PMap.create String.compare)

let log = Fcgi.log

let put key value =
  log "put %s ; %s" key value ;
  let m = PMap.add key [value] !data in
    data := m

let add key value =
  log "add %s ; %s" key value ;
  let old = PMap.find key !data in
  let value = List.rev (value::old) in
  let m = PMap.add key value !data in
    data := m


let get key =
  PMap.find key !data

let data () = 
  !data
