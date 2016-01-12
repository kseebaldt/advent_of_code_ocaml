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

let inc_bad_pair count a b =
  if is_naughty_pair (a, b) then count + 1 else count
;;

let inc_pair count a b = 
  if a = b then count + 1 else count
;;

let analyze_string s =
  let rec analyze_string (vowels, pairs, bad_pairs) cs = match cs with
    | [] -> (vowels, pairs, bad_pairs)
    | [x] -> (inc_vowel vowels x, pairs, bad_pairs)
    | h :: n :: t -> 
      let counts = (inc_vowel vowels h, inc_pair pairs h n, inc_bad_pair bad_pairs h n) in
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

