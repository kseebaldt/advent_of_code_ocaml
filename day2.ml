open Core.Std;;

let perimeter x y = 
  2 * x + 2 * y
;;

let calc_ribbon l w h =
  let r = (min (min (perimeter l w) (perimeter l h)) (perimeter w h)) in
    r + (l * w * h)
;;

let calc_paper l w h = 
  let s1 = l * h in
  let s2 = l * w in
  let s3 = w * h in
  2 * s1 + 2 * s2 + 2 * s3 + (min (min s1 s2) s3)
;;

let apply f list = match list with
  | [l; w; h] -> f l w h
  | _ -> 0
;;

let parse_line l = 
  List.map ~f:(fun s -> Int.of_string s) (String.split ~on:'x' l)
;;

let read_lines filename = In_channel.with_file filename ~f:(fun file ->
  In_channel.input_lines file)
;;

let calc ~f lines = 
  List.fold ~init:0 ~f:(fun amount line -> 
      amount + apply f (parse_line line)) lines
;;

let day2 filename =
  let lines = read_lines filename in 
  calc ~f:calc_paper lines
;;

let day2_2 filename = 
  let lines = read_lines filename in 
  calc ~f:calc_ribbon lines
;;

let run filename =
  Printf.printf "day2 : %d\n" (day2 filename);
  Printf.printf "day2_2 : %d\n" (day2_2 filename)
;;
