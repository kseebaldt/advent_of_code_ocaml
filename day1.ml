open Batteries;;

let dir = function
  | '(' -> 1
  | ')' -> (-1)
  | _ -> 0
;;

let day1 s = 
  String.fold_left (fun floor c -> floor + (dir c)) 0 s
;;

let day1_2 s =
  let rec day1_2 = function
    | ("", _, pos) -> pos
    | (_, (-1), pos) -> pos
    | (str, floor, pos) -> let c = String.get str 0 in
      let rest = (String.sub str 1 ((String.length str) - 1)) in
      let next_floor = floor + (dir c) in
      day1_2 (rest, next_floor, pos + 1) in
  day1_2 (s, 0, 0)

