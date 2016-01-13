open Core.Std;;

let coords str = 
  let [x; y] = String.split ~on:',' str in
  (Int.of_string x, Int.of_string y)
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

let turn_on v = 
  1
;;

let turn_off v = 
  0
;;

let toggle v = 
  (v + 1) mod 2
;;

let eval_tokens l a = match l with
  | "turn" :: "on" :: tl :: "through" :: br :: [] -> apply ~f:turn_on ~array:a tl br
  | "turn" :: "off" :: tl :: "through" :: br :: [] -> apply ~f:turn_off ~array:a tl br
  | "toggle" :: tl :: "through" :: br :: [] -> apply ~f:toggle ~array:a tl br
  | _ -> ()

;;

let eval_string s a =
  eval_tokens (String.split ~on:' ' s) a
;;

let read_lines filename = In_channel.with_file filename ~f:(fun file ->
  In_channel.input_lines file)
;;

let day6 filename = 
  let a = Array.create ~len:(1000 * 1000) 0 in
  let lines = read_lines filename in
  List.iter lines ~f:(fun s -> eval_string s a);
  Array.fold a ~init:0 ~f:(fun acc i -> acc + i)
;;

