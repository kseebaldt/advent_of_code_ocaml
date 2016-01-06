open Day1;;
open Batteries;;

let () =
  let input = Option.get (Enum.peek (File.lines_of "input1.txt")) in
  Printf.printf "day1 : %d\n" (day1 input);
  Printf.printf "day1_2 : %d\n" (day1_2 input)
;;

