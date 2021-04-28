(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2021 Ilya Sergey

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all
copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
SOFTWARE.
*)

open Util
open Rooms
open Polygons
open ArrayUtil

(*********************************************)
(*       Automated generation of rooms       *)
(*********************************************)

(*  size is the maximal span of the room along both X and Y dimensions *)
(*  Example generate_random_room 4 should return a room that fits into a
    4x4 square. *)

(*RUI: The idea is to generate a valid polygon, 
and use polygon_to_room to convert it to room*)
let generate_random_room (size : int) : room = 
  error "Implement me"


(* Define what it means to the room to be valid (e.g., no lacunas,
   obstacles, there is a place for initial Vroomba position, etc). *)

(* Linda: also check that the room edges only change either x or y? 
 * It's okay if this function's implementation-sepcific, I assume *)

(*RUI: A few checks:
1. No lacunas: O(n^2)
  Convert room to polygon -> every point in polygon must match the room's pos E or I 
2. No obstacles: O(n^2)
  Every vertical or horizontal line in the n x n square must have 0 or even number of 
  intersects with the room polygon or is collinear to the edge.
3. Initial place for Vroomba: O(1)
  (0,0) must have pos = E
  (1,1) must not be O
4. Consecutive edges only run horiontally or vertically:  O(n)
  only change in x or y coordinate
5. No points repeat O(n)
*)


let point_to_coor (Point (x, y)) = 
  (int_of_float x,int_of_float y)

let coor_to_point (x,y)=
  Point (float_of_int x, float_of_int y)

(*RUI: input coordinates; output pos
TODO: Change this for negative coordinates*)
let get_pos room (x,y) =
  room.map.(x).(y)

let polygon_to_int_pairs polygon = 
  let int_pairs = ref [] in 
  List.iter (fun p -> 
                    let (x,y) = point_to_coor p in 
                    int_pairs := (x,y):: !int_pairs) 
            polygon;
  List.rev !int_pairs



(* find the direction from point 1 to point 2*)
type direction = 
  |Up
  |Down
  |Left
  |Right
  |Diagonal
  |Stop

let find_direction p1 p2 = 
  let (x1, y1) = p1
  and (x2, y2) = p2 in 
  let (h, v) = ((x2 - x1), (y2 - y1)) in 
  if h = 0 
  then (if v = 0 then Stop else (if v>0 then Up else Down))
  else 
    (if h >0 
    then (if v = 0 then Right else (if v>0 then Diagonal else Diagonal))
    else (if v = 0 then Left else (if v>0 then Diagonal else Diagonal) ))
  
let valid_room (r: room) : bool = 
  let size = Array.length r.map in
  let len = List.length !(r.edges) in
  let polygon = room_to_polygon r in 
  let int_pairs = polygon_to_int_pairs polygon in
  let sort_by_x = 
      List.sort (fun (a,b) (c,d) ->
      if a > c then 1
      else (if a < c then -1 else 0)) 
      int_pairs
  in
    let sort_by_y = 
        List.sort (fun (a,b) (c,d) ->
        if b > d then 1
        else (if b < d then -1 else 0)) 
        int_pairs
  in
    let x_min = fst (List.hd sort_by_x) in
    let x_max = x_min + size - 1 in
    let y_min = fst (List.hd sort_by_y) in
    let y_max = y_min + size - 1
  in 

  (*get all the points of the rectangle that the room fits in *)
    let all_points = ref [] in
  for x = x_min to x_max do
      for y = y_min to y_max do 
        all_points := (x,y) :: !all_points
      done;
  done;

  (* check for lacunas *)
  let no_lacunas = 
    List.for_all (fun coor -> 
                  if 
                      (let p = coor_to_point coor in
                      point_within_polygon polygon p)
                  then 
                      (*points in polygon must not map to Outer*)
                      (if get_pos r coor = Outer 
                      then false 
                      else true)
                  else
                      (*points ouside polygon must map to Outer*)
                      (if get_pos r coor = Outer
                      then true 
                      else false)
                  )
                  !all_points
  in
    let no_straight_line = 
      if len <= 3 then false
      else
      begin
        let res = ref true in
        let arr = list_to_array int_pairs in 
        for i = 0 to len - 3 do 
          let d1 = find_direction arr.(i) arr.(i + 1)
          and d2 = find_direction arr.(i + 1) arr.(i + 2) in
          if d1 = d2 then res := false
          ;if not (!res) then let (x,y) = arr.(i) in Printf.printf "Found straight line (%d,%d)\n" x y
        done;
        (*check the last two + first point*)
        print_endline "Check last points";
        let d1 = find_direction arr.(len - 2) arr.(len - 1)
        and d2 = find_direction arr.(len - 1) arr.(0) in
        if d1 = d2 then res := false ;
        !res
      end

  in no_lacunas && no_straight_line;;






(*********************************************)
(*                     Tests                 *)
(*********************************************)

(*
let%test "Generated room is valid" = 
  let r = generate_random_room 100 in
  valid_room r
*)

(* TODO: add more tests *)
let%test "test_valid_room_simple" = 
  let input  = find_file "../../../resources/basic.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    let room = polygon_to_room p in  
                    valid_room room) 
  polygon_list

let%test "test_valid_room_simple_negative" = 
  let input  = find_file "../../../resources/invalid.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    print_endline "\n\nCHECKING POLYGON\n";
                    let room = polygon_to_room p in  
                    not (valid_room room)) 
  polygon_list;;

  let input  = find_file "resources/invalid.txt" in
  let polygon_list = file_to_polygons input in
  let p = List.hd polygon_list in
  let room = polygon_to_room p in  room;;
