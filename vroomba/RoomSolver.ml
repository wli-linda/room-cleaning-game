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
include RoomChecker
open RoomGenerator

open RoomUtil
open BetterHashTable
open Graphs
open LinkedGraphs

(*********************************************)
(*              Room solver                  *)
(*********************************************)

module CoorTable =
  ResizableListBasedHashTable(struct type t = (int * int) end)

(* This is a complex task. Feel free to introduce whatever functions
   and data types you consider necessary, and also rely on data
   structures and algorithms from the lectures (see folder `lib` of
   this project). *)

let inside_room r coor : bool =
  let (x, y) = coor in
  let left_bottom = get_pos r (x, y) in
  if left_bottom = Outer
  then false
  else begin
    (get_pos r (x + 1, y) != Outer &&
     get_pos r (x, y + 1) != Outer &&
     get_pos r (x + 1, y + 1) != Outer)
  end

let movable_coords r =
  let ls = ref [] in
  let map = r.map in
  let len = Array.length map in
  for x = 0 to len - 2 do
    for y = 0 to len - 2 do
      if inside_room r (x, y)
      then ls := (x, y) :: !ls
    done
  done;
  !ls

let init_state r =
  let ls = movable_coords r in
  let num = List.length ls in
  let ht = HygieneTable.mk_new_table num in
  List.iter (fun coor -> HygieneTable.insert ht coor Dirty) ls;
  let start = (0, 0) in
  { current = ref start;
    table = ht;
    dirty_tiles = ref num }

let get_id ct coor =
  CoorTable.get ct coor

let add_edges g ct coor =
  let add_edge src dst_op =
    if dst_op != None
    then (let dst = get_exn dst_op in
          add_edge g src dst;
          set_edge_label g src dst 1) in
  let (x, y) = coor in
  let id = get_exn @@ get_id ct coor in
  let up = get_id ct (x, y + 1) in
  let left = get_id ct (x - 1, y) in
  let down = get_id ct (x, y - 1) in
  let right = get_id ct (x + 1, y) in
  add_edge id up;
  add_edge id left;
  add_edge id down;
  add_edge id right

let create_graph r =
  let g = mk_graph () in
  let ls = movable_coords r in
  let ct = CoorTable.mk_new_table (List.length ls) in
  List.iter (fun coor ->
      CoorTable.insert ct coor !(g.next_node_id);
      add_node g coor) ls;
  List.iter (fun coor -> add_edges g ct coor) ls;
  g

(* Solve the room and produce the list of moves. *)
(* Make use of RoomChecker.state state type internally in your solver *)
let solve_room (r: room) : move list =
  (* TODO: CREATING A GRAPH
   * find all coordinates in the room the vroomba can move to,
   * add nodes for all these coordinates, along with edge label of 1
   * create edges for all possible moves, found with hash_table *)


  (* TODO: DFS & BACKTRACKING *)

  
  error "Implement me!"

(*********************************************)
(*               Testing                     *)
(*********************************************)

(*
let%test "Randomised solver testing" = 
  let r = generate_random_room 30 in
  let moves = solve_room r in
  check_solution r moves
 *)

(* TODO: Add more tests *)
