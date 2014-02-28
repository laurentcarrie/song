open ExtString

let utf8_of_code codes = String.implode ( List.map Char.chr codes )

let bemol = utf8_of_code [ 0xE2 ; 0x99 ; 0xAD ]

