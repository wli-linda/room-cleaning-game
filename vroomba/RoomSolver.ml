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

module ReachTable =
  ResizableListBasedHashTable(struct type t = int end)

type reached = White | Black
    
(* This is a complex task. Feel free to introduce whatever functions
   and data types you consider necessary, and also rely on data
   structures and algorithms from the lectures (see folder `lib` of
   this project). *)
    
let get_pos_map_index r (x, y) =
  let map = r.map in
  map.(x).(y)

let movable_coords r= 
  get_all_tiles r

let reachable' state coor neighbor = 
  let (a, b) = coor in 
  let (c, d) = neighbor in
  let (dx, dy) = (c - a, d - b) in
  let ht = state.table in
  (* the neighbor is in the room *)
  let op = HygieneTable.get ht neighbor in
  op != None &&
  begin
    (* the neighbor is next-door -> reachable *)
    ((abs dx = 1 && dy = 0) || (dx = 0 && abs dy = 1)) ||
    (* the neighbor is diagonal to current tile *)
    begin 
      (HygieneTable.get ht (a, d) != None) &&
      (HygieneTable.get ht (c, b) != None)
    end
  end
  
let clean state curr =
  clean_a_tile state curr;
  let neighbors = get_eight_neighbors curr in
  let ht = state.table in
  List.iter (fun coor ->
      let op = HygieneTable.get ht coor in
      if op != None
      then (if reachable' state curr coor
            then clean_a_tile state coor)
    ) neighbors
    
let init_state r =
  let ls = movable_coords r in
  let num = List.length ls in
  let ht = HygieneTable.mk_new_table num in
  List.iter (fun coor -> HygieneTable.insert ht coor Dirty) ls;
  let start = (0, 0) in
  let state = 
    { current = ref start;
      table = ht;
      dirty_tiles = ref num } in
  clean state start;
  state

let%test "test_reachable'" = 
  let s = "(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  let state = init_state room in
  reachable' state (0, 0) (0, 1) &&
  not (reachable room (0, 0) (1, 1)
  ) 

let get_id ct coor = CoorTable.get ct coor
    
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
  let rt = ReachTable.mk_new_table (List.length ls) in
  List.iter (fun coor ->
      CoorTable.insert ct coor !(g.next_node_id);
      ReachTable.insert rt coor White;
      add_node g coor) ls;
  List.iter (fun coor -> add_edges g ct coor) ls;
  (g, ct, rt)
    
let moves_to_string ls =
  let buffer = Buffer.create 1 in
  List.iter (fun m -> Buffer.add_string buffer (pp_move m)) ls;
  Buffer.contents buffer
    
(* Solve the room and produce the list of moves. *)
(* Make use of RoomChecker.state state type internally in your solver *)
let solve_room (r: room) : move list =
  let (g, ct, rt) = create_graph r in
  let state = init_state r in
  let ht = state.table in
  let moves = ref [] in
  let init_coor = get_exn @@ get_id ct !(state.current) in
  let get_coor g id = get_value @@ get_node g id in
  
  let rec check_hygiene ls =
    match ls with
    | [] -> None
    | h :: tl ->
      let coor = get_coor g h in
      let is_cleaned = get_exn @@ HygieneTable.get ht coor in
      if is_cleaned = Dirty
      then Some h
      else check_hygiene tl
  in 

  (* TODO: DFS & BACKTRACKING *)
  let rec dfs_visit id =
    clean state (get_coor g id);
    ReachTable.insert rt (get_coor g id) Black;
    if !(state.dirty_tiles) = 0
    then List.rev !moves
    else begin
      let new_move = ref true in
      let rec walk_succ_ls id_walk succ_ls ls_moved =
        if !(state.dirty_tiles) = 0
        then List.rev !moves
        else begin
          let (x, y) = get_coor g id_walk in
          match succ_ls with
          | [] ->
            if !new_move = true
            then (new_move := false; backtrack !moves id_walk)
            else backtrack ls_moved id_walk
          | h :: tl ->
            let coor = get_coor g h in
            if (get_exn @@ ReachTable.get rt coor = Black &&
                (let succ_succ_ls = get_succ g h in
                 let op = check_hygiene succ_succ_ls in
                 op = None))
            then walk_succ_ls id_walk tl ls_moved 
            else begin
              new_move := true;
              let (x', y') = coor in
              if x = x' && y + 1 = y' then moves := RoomChecker.Up :: !moves
              else if x - 1 = x' && y = y' then moves := Left :: !moves
              else if x = x' && y - 1 = y' then moves := Down :: !moves
              else if x + 1 = x' && y = y' then moves := Right :: !moves;
              dfs_visit h
            end
        end
      and backtrack ls_moved id_bk =
        if !(state.dirty_tiles) = 0
        then List.rev !moves
        else begin
          match ls_moved with
          | [] -> !moves (* shouldn't reach *)
          | h :: tl ->
            let (x, y) = get_coor g id_bk in
            let (coor', rev_move) = match h with
              | RoomChecker.Up -> ((x, y - 1), RoomChecker.Down)
              | Left -> ((x + 1, y), Right)
              | Down -> ((x, y + 1), Up)
              | Right -> ((x - 1, y), Left) in
            moves := rev_move :: !moves;
            let id' = get_exn @@ get_id ct coor' in
            let succ_ls' = get_succ g id' in
            walk_succ_ls id' succ_ls' tl
        end
      in let succ_ls = get_succ g id in
      walk_succ_ls id succ_ls []
    end
  in dfs_visit init_coor
    
let solve_runner input_file output_file =
  let polygon_ls = file_to_polygons input_file in
  let res = ref [] in
  List.iter (fun p ->
      let r = polygon_to_room p in
      let moves = solve_room r in
      let s = moves_to_string moves in
      res := s :: !res) polygon_ls;
  BinaryEncodings.write_strings_to_file output_file (List.rev !res)

    
(*********************************************)
(*               Testing                     *)
(*********************************************)
    
let%test "Basic room solver testing 1" =
  let ls = [(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)] in
  let r = Polygons.polygon_of_int_pairs ls |> polygon_to_room in
  let moves = solve_room r in
  check_solution r moves
    (*
let%test "Basic room solver testing 2" =
  let ls = [(0, 0); (2, 0); (2, 2); (0, 2)] in
  let r = Polygons.polygon_of_int_pairs ls |> polygon_to_room in
  let moves = solve_room r in
  check_solution r moves && moves = []
                            *)        
let%test "Basic room solver testing 3" =
  let ls = [(0, 0); (1, 0); (1, 1); (2, 1); (2, 2); (0, 2)] in
  let r = Polygons.polygon_of_int_pairs ls |> polygon_to_room in
  let moves = solve_room r in
  check_solution r moves
    
let%test "Basic room solver testing with rooms.txt" =
  let input  = BinaryEncodings.find_file "../../../resources/rooms.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p ->
      let r = polygon_to_room p in
      let moves = solve_room r in
      check_solution r moves) polygon_list
      
let%test "Randomised solver testing 1" = 
  let r = generate_random_room 10 in
  let moves = solve_room r in
  check_solution r moves

let%test "Randomised solver testing 2" = 
  let r = generate_random_room 30 in
  let moves = solve_room r in
  check_solution r moves
 
let%test "Randomised solver testing 3" = 
  let r = generate_random_room 100 in
  let moves = solve_room r in
  check_solution r moves

let%test "Randomised solver testing 4" = 
  let input  = BinaryEncodings.find_file "../../../resources/test_generate_l.txt" in
  let polygon_list = file_to_polygons input in
  List.for_all (fun p ->
      let r = polygon_to_room p in
      let moves = solve_room r in
      check_solution r moves) polygon_list
