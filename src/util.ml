open Data

let int_of_string s =
  try int_of_string s with | e -> failwith("cannot convert '"^s^"' to an int")



let update_digest d s =
  let d2 = Digest.file s in
    Digest.string ( (Digest.to_hex d) ^ (Digest.to_hex d2))
