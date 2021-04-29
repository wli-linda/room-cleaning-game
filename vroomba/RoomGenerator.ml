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
1. No lacunas: 
  In our implementation, drawing a lacuna will end up with edges extending out from 
  existing edges that not adjacent to it. So here we just need to check if edge intersect
  with another edge that is not adjacent to it
2. No obstacles : O(n^2)
  Same as above

3. Initial place for Vroomba: O(1)
  (0,0) must not be Outer
  (1,1) must not be Outer
4. No diagonal edges: 
  Consecutive edges only run horiontally or vertically
  only change in x or y coordinate
  *this will throw error in polygon_to_room
6. No straight lines :
  3 consecutive edges on the same line are not allowed
7. No collinear edges:
  Check for non-collinearity while checking #1
8. No "8" shaped rooms:
  same checks as #1
   
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


let valid_room (r: room) : bool = 
  let polygon = room_to_polygon r in 
  let len = List.length !(r.edges) in
  if len <= 3 then false 
  else
  begin

  (*get edge list in Point pairs *)
  let edge_list = Polygons.edges polygon in
  let edge_arr = list_to_array edge_list in

  let no_intersect_or_collinear = 
    let res = ref true in 

    let i = ref 0 in 
    while !res && !i < len - 1 do
      let s1 = edge_arr.(!i) in 

      (* s1 vs all segments behind it excluding the next neighbor *)
      begin
      for j = !i+2 to len - 1 do
        (* avoid index out of bounds *)
        if j <= len - 1 then
        begin
        (* skip first and last segment comparison *)
        if !i = 0 && j = len -1 then () 
          else
            (let s2 = edge_arr.(j) in
            if (segments_intersect s1 s2) ||
                (intersect_as_collinear s1 s2)
            then res := false 
            else ())  
        end
      done
      end;

      (* s1 vs all segments before it excluding the previous neighbor *)
      begin
      for j = !i-2 downto 0 do
        (* avoid index out of bounds *)
        if j >= 0 then
        begin
        (* skip first and last segment comparison *)
        if !i = len -1 && j = 0 then () 
        else
          (let s2 = edge_arr.(j) in
          if (segments_intersect s1 s2) ||
              (intersect_as_collinear s1 s2)
          then res := false)
        end
        done
      end;
      i := !i + 1
    done;
    !res
  in

    let no_straight_line = 
      let res = ref true in 
      let i = ref 0 in
      (*check first to the second last segment*)
      while !res && !i < len -1 do
        let s1 = edge_arr.(!i)
        and s2 = edge_arr.(!i+1) in
        res := not (on_straight_line s1 s2);
        i := !i + 1
      done ;

      (*check first and last segment*)
      let first_seg = edge_arr.(0)
      and last_seg = edge_arr.(len - 1) in 
      res := not (on_straight_line first_seg last_seg) ;
      !res
  in

    let space_for_vroomba = 
      get_pos r (0, 0) != Outer && get_pos r (1, 1) != Outer

  in no_intersect_or_collinear && no_straight_line && space_for_vroomba
end





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
  let input  = BinaryEncodings.find_file "../../../resources/basic.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    let room = polygon_to_room p in  
                    valid_room room) 
  polygon_list

(* (0, 0); (0, 2); (-2, 2); (-2, -3); (3, -3); (3, 0)
(0, 0); (4, 0); (4, 4); (0, 4); (0, 0); (-4, 0); (-4, -4); (0, -4) *)
let%test "test_valid_room_simple_negative" = 
  let input  = BinaryEncodings.find_file "../../../resources/invalid.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    (* print_endline "\n\nCHECKING POLYGON\n"; *)
                    try (let room = polygon_to_room p in  
                    not (valid_room room))
                    with Failure _ -> true) 
  polygon_list