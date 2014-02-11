open Data

let int_of_string s =
  try int_of_string s with | e -> failwith("cannot convert '"^s^"' to an int")

