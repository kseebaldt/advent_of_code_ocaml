open Core.Std;;

let calc_digest s = Digest.to_hex (Digest.string s);;

let find_digest s len = 
  let prefix = String.make len '0' in
  let rec find_digest s n =
    let digest = calc_digest (s ^ Int.to_string n) in
    if (String.slice digest 0 len) = prefix then n else
      find_digest s (n + 1) in
  find_digest s 0
;;


