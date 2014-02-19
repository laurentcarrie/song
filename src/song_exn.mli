val push_message : string -> int -> string -> unit
val push_message_fmt : string -> int -> ('a, unit, string, unit) format4 -> 'a 
val clear_stack : unit -> unit
val print_stack : unit -> unit
val string_of_stack : unit -> string
val html_string_of_stack : unit -> string
val set_debug : bool -> unit
val get_stack : unit -> (string*int*string) list
