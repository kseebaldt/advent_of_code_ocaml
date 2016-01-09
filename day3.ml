open Batteries;;

module House = struct
  type t = int * int
  let compare (x0,y0) (x1,y1) =
    match Pervasives.compare x0 x1 with
    | 0 -> Pervasives.compare y0 y1
    | c -> c
end

module HouseSet = Set.Make(House)

let next_house (x, y) c = match c with
  | '<' -> (x - 1, y)
  | '^' -> (x, y - 1)
  | '>' -> (x + 1, y)
  | 'v' -> (x, y + 1)
  | _ -> (x, y)
;;

let shift item l = match l with
  | [] -> [item]
  | h :: t -> t @ [item]
;;

let rec repeat n item = match n with
  | 0 -> []
  | n -> item :: (repeat (n-1) item)
;;

let visit n string =
  let start = repeat n (HouseSet.add (0,0) HouseSet.empty, (0,0)) in
  let final = Enum.fold (fun l c ->
      let (set, house) = List.hd l in
      let next = next_house house c in
      shift ((HouseSet.add next set), next) l) start (String.enum string) in
  let set = List.fold_left (fun acc (set, _) -> HouseSet.union acc set) HouseSet.empty final in
  HouseSet.cardinal set
;;

let day3 string =
  visit 1 string
;;

let day3_2 string =
  visit 2 string
;;
