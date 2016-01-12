open Core.Std;;

let is_vowel c = match c with
  | 'a' | 'e' | 'i' | 'o' | 'u' -> true
  | _ -> false
;;

let is_naughty_pair p = match p with
  | ('a', 'b') | ('c', 'd') | ('p', 'q') | ('x', 'y') -> true
  | _ -> false
;;

let inc_vowel v c = 
  if is_vowel c then v + 1 else v
;;

let inc_pair count a b = 
  if a = b then count + 1 else count
;;

let analyze_string s =
  let rec analyze_string (vowels, pairs, bad_pairs) cs = match cs with
    | [] -> (vowels, pairs, bad_pairs)
    | [x] -> (inc_vowel vowels x, pairs, bad_pairs)
    | h :: n :: t -> 
      if is_naughty_pair (h, n) then (0, 0, 1) else
      let counts = (inc_vowel vowels h, inc_pair pairs h n, 0) in
      analyze_string counts (n :: t) in
  analyze_string (0, 0, 0) (String.to_list s)
;;

let is_nice s =
  let (vowels, pairs, bad_pairs) = analyze_string s in
  vowels > 2 && pairs > 0 && bad_pairs = 0
;;

let read_lines filename = In_channel.with_file filename ~f:(fun file ->
  In_channel.input_lines file)
;;

let day5 filename =
  let lines = read_lines filename in
  List.length (List.filter ~f:is_nice lines)
;;

let run filename = 
  Printf.printf "day5: %d\n" (day5 filename)
;;

