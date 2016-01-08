open Day3;;
open Batteries;;

let () =
  let input = Option.get (Enum.peek (File.lines_of "input3.txt")) in
  Printf.printf "day3 : %d\n" (day3 input)
  (* Printf.printf "day2_2 : %d\n" (day2_2 (File.lines_of "input2.txt")) *)
;;

