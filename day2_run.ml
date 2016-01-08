open Day2;;
open Batteries;;

let () =
  Printf.printf "day2 : %d\n" (day2 (File.lines_of "input2.txt"));
  Printf.printf "day2_2 : %d\n" (day2_2 (File.lines_of "input2.txt"))
;;

