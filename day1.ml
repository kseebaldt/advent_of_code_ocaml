open Core.Std;;

let dir = function
  | '(' -> 1
  | ')' -> (-1)
  | _ -> 0
;;

let day1 s = 
  String.fold ~init:0 ~f:(fun floor c -> floor + (dir c)) s
;;

let day1_2 s =
  let rec day1_2 = function
    | ("", _, pos) -> pos
    | (_, (-1), pos) -> pos
    | (str, floor, pos) -> 
      let c = String.get str 0 in
      let rest = (String.sub str 1 ((String.length str) - 1)) in
      let next_floor = floor + (dir c) in
      day1_2 (rest, next_floor, pos + 1) in
  day1_2 (s, 0, 0)
;;

let read_lines filename = In_channel.with_file filename ~f:(fun file ->
  In_channel.input_lines file)
;;

let run filename =
  let input = List.hd_exn (read_lines filename) in
  Printf.printf "day1 : %d\n" (day1 input);
  Printf.printf "day1_2 : %d\n" (day1_2 input)
;;
