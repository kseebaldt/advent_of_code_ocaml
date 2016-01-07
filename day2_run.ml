open Day2;;
open Batteries;;

let () =
  let input = File.lines_of "input2.txt" in
  Printf.printf "day2 : %d\n" (day2 input)
;;

