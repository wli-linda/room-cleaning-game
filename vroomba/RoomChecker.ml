
(*
This file is part of teaching material of Yale-NUS College module
"YSC2229: Introductory Data Structures and Algorithms"

Copyright (c) 2020 Ilya Sergey

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
open Graphs
open BetterHashTable
open RoomGenerator
open RoomUtil

module HygieneTable = 
  ResizableListBasedHashTable(struct type t = (int * int) end)

(*********************************************)
(*         Movements of Vroomba              *)
(*********************************************)

type move = 
  | Up 
  | Left 
  | Down 
  | Right

(* Print the move *)
let pp_move = function
  | Up -> "W"
  | Left -> "A"
  | Down -> "S"
  | Right -> "D"

(* This is a data type, representing the state of the room at a
   certain point of cleaning. It should include the room and keep
   track of Vroomba's current position, parts that are already cleaned
   and that are remaining to be cleaned. Use this data type internally
   in the function `check_solution` *)

type hygiene = 
  | Dirty
  | Clean

type state = {
 current : (int * int) ref ;
 table : ((int * int) * hygiene) HygieneTable.hash_table;
 dirty_tiles : int ref
}



let initiate_state room =
  let num = get_tiles_num room in
  let all_points = get_all_points room in
  let ht = HygieneTable.mk_new_table num in
  List.iter (fun coor -> HygieneTable.insert ht coor Dirty) all_points;
  let starting_point = (0,0) in
  { 
    current  = ref starting_point;
    table =  ht;
    dirty_tiles = ref num
  }
  
let%test "test_initial_state" =
  let input  = BinaryEncodings.find_file "../../../resources/basic.txt" in
  let polygon_list = file_to_polygons input in
  let p = List.hd polygon_list in
  let room = polygon_to_room p in 
  let state = initiate_state room in
  !(state.current) = (0,0) &&
  !(state.dirty_tiles) = 20


(*********************************************)
(*            Checking solution              *)
(*********************************************)

(*  Get a trace of Vroomba from a string  *)
(*  A string can be ill-formed! *)
let string_to_solution (s: string) : move list option = 
  let len = String.length s in
  let res = ref [] in
  try (for i = 0 to len - 1 do
         let move = 
           match s.[i] with
           | 'W' -> Up
           | 'A' -> Left
           | 'S' -> Down
           | 'D' -> Right
           | _ -> error "Unrecognizable move!" in
         res := move :: !res
       done;
       Some (List.rev !res))
  with error ->
    None

let move_in_dir coor dir =
  let (x, y ) = coor in
  match dir with 
  | Up -> (x , y + 1)
  | Down -> (x , y - 1)
  | Left -> (x - 1 , y)
  | Right -> (x + 1 , y)

(* RUI: 
When cleaning a tile:
Mark the hygeine status as Clean only if the tile is Dirty

1. At current location, check the coordinates:
  - If does not exist in room -> fail game
  - If exists:
    - The tile is not cleanable: fail game
    - The tile is cleanable: change hygeine state to Clean in hashtable & dirty_tiles -1
2. Check the 8 neighbouring coordinates:
  - If any coordinate does not exist in room -> ignore
  - For those that are in room:
    - If not cleanable: ignore
    - If cleanable & reacheable: clean
3. Move the current point

 *)


let clean_a_tile state coor = 
  let ht = state.table in 
  let hg = get_exn (HygieneTable.get ht coor) in
  if hg = Dirty
  then
  state.dirty_tiles := !(state.dirty_tiles) - 1;
  HygieneTable.insert ht coor Clean


(*  Check that the sequence of moves is valid  *)
let check_solution (r: room) (moves: move list) : bool = 
  let move_list = ref moves in
  let remaining = ref ((List.length moves) + 1) in
  let state = initiate_state r in 
  let success = ref true in
  while (!success && !remaining > 0) do
    begin

    (* Check current point *)
    let curr = !(state.current) in
    begin
    if not (exist_in_room r curr)
    then success := false
    else 
      (if not (cleanable r curr)
      then success := false 
      else clean_a_tile state curr
      ) 
    end ;

    (* Check the eight neighbors *)
    let neighbors = get_eight_neighbors curr in
    List.iter (fun coor -> 
              if exist_in_room r coor
              then 
                (if reachable r curr coor
                then clean_a_tile state coor)
              )
              neighbors;

    (* move Vroomba and update move_list  *)
    remaining := !remaining - 1;
    if !remaining > 0
    then
      (let dir = List.hd !move_list in
      state.current := move_in_dir curr dir;
      move_list := List.tl !move_list;
      )
    end
  done;
  if !(state.dirty_tiles) > 0 
  then false
  else !success


(*  Top-level validator  *)
let validate r s = 
  match string_to_solution s with
  | None -> false
  | Some moves -> check_solution r moves
 

let%test _ = 
  let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "WDDDDDD" 

(* TODO: Add more tests *)

let%test "test_checker_simple 1" = 
  let s = "(0, 0); (1, 0); (1, 1); (0, 1)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "" 

let%test "test_checker_simple 2" = 
  let s = "(0, 0); (2, 0); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "" 

let%test "test_checker_simple 3" = 
  let s = "(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "W" 

let%test "test_checker_simple 3 neg" = 
  let s = "(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  not (validate room "") 

let%test "test_checker_simple 4" = 
  let s = "(0, 0); (0, 3); (1, 3); (1, 0)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "W" 

let%test "test_checker_simple 4 neg" = 
  let s = "(0, 0); (0, 3); (1, 3); (1, 0)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  not (validate room "") 

let%test "test_checker_basic_negative" = 
  let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  not (validate room "WWWWDDDDD") &&
  not (validate room "WWDDAD")
(* 
let%test "test_checker_rooms_negative" = 
  let input  = BinaryEncodings.find_file "../../../resources/rooms.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    let room = polygon_to_room p in 
                    not (validate room "W") ) 
  polygon_list

let%test "test_checker_rooms_negative" = 
  let input  = BinaryEncodings.find_file "../../../resources/rooms.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p -> 
                    let room = polygon_to_room p in 
                    not (validate room "W") ) 
  polygon_list

let%test "test_checker_random_negative" = 
  let room = generate_random_room 100 in
  not (validate room "W") *)