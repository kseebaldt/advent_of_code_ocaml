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

let day3 string =
  let (s, h) = Enum.fold (fun (set, house) c ->
      let next = next_house house c in
      ((HouseSet.add next set), next)) (HouseSet.empty, (0,0)) (String.enum string)
  in
  HouseSet.cardinal s
;;
  

