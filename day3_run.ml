open Day3;;
open Batteries;;

let () =
  Printf.printf "day3 : %d\n" (day3 (Option.get (Enum.peek (File.lines_of "input3.txt"))));
  Printf.printf "day3_2 : %d\n" (day3_2 (Option.get (Enum.peek (File.lines_of "input3.txt"))));
;;

