open Core.Std;;

type day6_functions = {
  turn_on : int -> int ;
  turn_off : int -> int ;
  toggle : int -> int ;
};;

let coords str = 
  let ints = List.map (String.split ~on:',' str) ~f:Int.of_string in
  (List.hd_exn ints, List.hd_exn (List.tl_exn ints))
;;

let apply ~f:f ~array:a tl br =
  let (x1, y1) = coords tl in
  let (x2, y2) = coords br in
  for j = y1 to y2 do
    for i = x1 to x2 do
      let pos = j * 1000 + i in
      a.(pos) <- f a.(pos)
    done
  done
;;

let eval_tokens funs l a = match l with
  | "turn" :: "on" :: tl :: "through" :: br :: [] -> apply ~f:funs.turn_on ~array:a tl br
  | "turn" :: "off" :: tl :: "through" :: br :: [] -> apply ~f:funs.turn_off ~array:a tl br
  | "toggle" :: tl :: "through" :: br :: [] -> apply ~f:funs.toggle ~array:a tl br
  | _ -> ()

;;

let eval_string funs s a =
  eval_tokens funs (String.split ~on:' ' s) a
;;

let read_lines filename = In_channel.with_file filename ~f:(fun file ->
  In_channel.input_lines file)
;;

let eval_lines funs lines =
  let a = Array.create ~len:(1000 * 1000) 0 in
  List.iter lines ~f:(fun s -> eval_string funs s a);
  Array.fold a ~init:0 ~f:(fun acc i -> acc + i)
;;

let part1 = {
  turn_on = (fun _ -> 1);
  turn_off = (fun _ -> 0);
  toggle = (fun x -> (x + 1) mod 2)
};;

let part2 = {
  turn_on = (fun x -> x + 1);
  turn_off = (fun x -> if (x - 1) < 0 then 0 else x - 1);
  toggle = (fun x -> x + 2)
};;

let day6 funs filename = 
  let lines = read_lines filename in
  eval_lines funs lines
;;

let run filename = 
  Printf.printf "day6: %d\n" (day6 part1 filename);
  Printf.printf "day6_2: %d\n" (day6 part2 filename)
;;
