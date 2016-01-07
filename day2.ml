open Batteries;;

let calc_paper l w h = 
  let s1 = l * h in
  let s2 = l * w in
  let s3 = w * h in
  2 * s1 + 2 * s2 + 2 * s3 + (min (min s1 s2) s3)
;;

let calc_paper_list list = match list with
  | [l; w; h] -> calc_paper l w h
  | _ -> 0
;;

let parse_line l = 
  List.map (fun s -> String.to_int s) (String.nsplit l "x")
;;

let day2 lines = 
  Enum.fold (fun amount line -> 
      amount + calc_paper_list (parse_line line)) 0 lines
;;


