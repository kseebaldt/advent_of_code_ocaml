open Core.Std;;

let next_house (x, y) c = match c with
  | '<' -> (x - 1, y)
  | '^' -> (x, y - 1)
  | '>' -> (x + 1, y)
  | 'v' -> (x, y + 1)
  | _ -> (x, y)
;;

let shift item l = match l with
  | [] -> [item]
  | _ :: t -> t @ [item]
;;

let rec repeat n item = match n with
  | 0 -> []
  | n -> item :: (repeat (n-1) item)
;;

let visit n string =
  let start = repeat n (Set.add Set.Poly.empty (0,0), (0,0)) in
  let final = List.fold ~init:start ~f:(fun l c ->
      let (set, house) = List.hd_exn l in
      let next = next_house house c in
      shift ((Set.add set next), next) l) (String.to_list string) in
  let set = List.fold ~init:Set.Poly.empty ~f:(fun acc (set, _) -> Set.union acc set) final in
  Set.length set
;;

let read_lines filename = In_channel.with_file filename ~f:(fun file ->
  In_channel.input_lines file)
;;

let day3 filename =
  let s = List.hd_exn (read_lines filename) in
  visit 1 s
;;

let day3_2 filename =
  let s = List.hd_exn (read_lines filename) in
  visit 2 s
;;

let run filename =
  Printf.printf "day3 : %d\n" (day3 filename);
  Printf.printf "day3_2 : %d\n" (day3_2 filename)
;;

