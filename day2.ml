open Batteries;;

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
  List.map (fun s -> String.to_int s) (String.nsplit l "x")
;;

let day2 lines = 
  Enum.fold (fun amount line -> 
      amount + apply calc_paper (parse_line line)) 0 lines
;;

let day2_2 lines = 
  Enum.fold (fun amount line -> 
      amount + apply calc_ribbon (parse_line line)) 0 lines
;;
