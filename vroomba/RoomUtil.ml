(* Helper functions *)

open Util
open Rooms
open Polygons

(* ========Point - Coordinate - Polygon - pos=======*)

let point_to_coor (Point (x, y)) = 
  (int_of_float x,int_of_float y)

let coor_to_point (x,y)=
  Point (float_of_int x, float_of_int y)

(*RUI: input coordinates; output pos
TODO: Change this for negative coordinates*)
let get_pos room (x,y) =
  room.map.(x).(y)

(* convert coordinates to room.map indices *)
let coor_to_map_index room (x,y) = 
  (x, y)

let map_index_to_coor room (x,y)  = 
  (x, y)

(* convert a list of points to a list of (int * int) *)
let polygon_to_int_pairs polygon = 
  let int_pairs = ref [] in 
  List.iter (fun p -> 
                    let (x,y) = point_to_coor p in 
                    int_pairs := (x,y):: !int_pairs) 
            polygon;
  List.rev !int_pairs

type direction = 
  |Up
  |Down
  |Left
  |Right
  |Diagonal
  |Stop

let find_direction p1 p2 = 
  let (x1, y1) = point_to_coor p1
  and (x2, y2) = point_to_coor p2 in 
  let (h, v) = ((x2 - x1), (y2 - y1)) in 
  if h = 0 
  then (if v = 0 then Stop else (if v>0 then Up else Down))
  else 
    (if h >0 
    then (if v = 0 then Right else (if v>0 then Diagonal else Diagonal))
    else (if v = 0 then Left else (if v>0 then Diagonal else Diagonal) ))

let print_segment s = 
  let p1, p2 = s in 
  let (x1, y1) = point_to_coor p1 in 
  let (x2, y2) = point_to_coor p2 in 
  Printf.printf "segment (%d, %d), (%d, %d)\n" x1 y1 x2 y2 

let print_segment_list l = 
  List.iter (fun s -> print_segment s) l

let on_straight_line s1 s2= 
    let (p1, p2) = s1
    and (p3, p4) = s2 in
    let d1 = find_direction p2 p1
    and d2 = find_direction p4 p3 in
    d1 = d2



(* ========Room coordinates - tiles - reachability=======*)

(*get all coordinates of the room & outside space*)
let get_all_points room =
  let map = room.map in 
  let len = Array.length map in
  let all_points = ref [] in 
  for x = 0 to len - 1 do 
    for y = 0 to len - 1 do 
    let coor = map_index_to_coor room (x,y) in
    all_points := coor :: !all_points
    done
  done;
  !all_points

 (* get the number of tiles in the room *)
let get_tiles_num room : int = 
  let tiles = ref 0 in
  let map = room.map in
  let len = Array.length map in
  for x = 0 to len - 2 do 
    for y = 0 to len - 2 do 
      let p1 = map.(x).(y) in 
      let p2 = map.(x).(y + 1) in 
      let p3 = map.(x + 1).(y) in 
      let p4 = map.(x + 1).(y + 1) in
      if p1 != Outer && p2 != Outer && 
        p3 != Outer && p4 != Outer
      then tiles := !tiles + 1
    done;
  done;
  !tiles


(*retrieve the coordinates of the points in the same tile*)
let get_three_neighbors (x, y) = 
  let n1 = (x, y + 1)
  and n2 = (x + 1, y)
  and n3 = (x + 1, y +1) in
  [n1; n2; n3]

(*retrieve the coordinates of the neighboring tiles*)
let get_eight_neighbors (x, y) = 
  let n1 = (x, y + 1)
  and n2 = (x + 1, y)
  and n3 = (x + 1, y + 1)
  and n4 = (x + 1, y - 1)
  and n5 = (x, y - 1)
  and n6 = (x - 1, y - 1)
  and n7 = (x - 1, y)
  and n8 = (x - 1, y + 1)
  in
  [n1; n2; n3; n4; n5; n6; n7; n8]


  (*if a coordinate exists in the room space*)
let exist_in_room room coor = 
  let all_points = get_all_points room in
  List.mem coor all_points 


(*is a tile at coor cleanable? aka. is a room tile*)
let cleanable room coor : bool =
  let p = get_pos room coor in 
  match p with 
  | Outer -> false
  | Inner -> true
  | Edge -> 
  let neighbours = get_three_neighbors coor in 
  List.for_all (fun n ->  
                  (exist_in_room room n) && 
                  let np = get_pos room n 
                    in not (np = Outer)  ) 
               neighbours

(* is a neighbor tile reachable from the current tile*)
let reachable room coor neighbor = 
  let (a, b) = coor in 
  let (c, d) = neighbor in
  let (dx, dy) = (c-a, d-b) in 
  (*four next-door neighbors are reachable*)
  if (abs dx = 1 && dy = 0) || (dx = 0 && abs dy = 1)
  then true
  else
  begin 
  (cleanable room (a, d)) &&
  (cleanable room (c, b))
  end


(* ========Tests=======*)

let%test "test_get_tiles_num" =
  let input  = BinaryEncodings.find_file "../../../resources/basic.txt" in
  let polygon_list = file_to_polygons input in
  let p = List.hd polygon_list in
  let room = polygon_to_room p in 
  let num = get_tiles_num room in
  num = 20


let%test "test_get_all_points" =
  let input  = BinaryEncodings.find_file "../../../resources/basic.txt" in
  let polygon_list = file_to_polygons input in
  let p = List.hd polygon_list in
  let room = polygon_to_room p in 
  let all_points = get_all_points room in
  let num = List.length all_points in 
  num = 81

let%test "test_reachable" = 
  let s = "(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  reachable room (0, 0) (0, 1) &&
  not (reachable room (0, 0) (1, 1))