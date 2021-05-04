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

include Util
open ArrayUtil
open Rooms
open RoomChecker
open RoomGenerator
open RoomSolver
open RoomUtil
open GraphicUtil
open ReadingFiles
include Polygons


(*********************************************)
(*           Gamifying the solver            *)
(*********************************************)

(* TODO: Implement more functions! *)

let write_solution_to_file (moves : move list) (path : string) : unit = 
  let buffer = Buffer.create 1 in
  List.iter (fun e -> Buffer.add_string buffer (pp_move e)) moves;
  ReadingFiles.write_string_to_file path (Buffer.contents buffer)

(* TODO: feel free to modify this function to add more parameters
   necessary for tracking your game state *)
let rec wait_until_q_pressed r =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' then close_graph () else
  if event.key == 'w' then () else
  if event.key == 'd' then () else
  if event.key == 'a' then () else
  if event.key == 'd' then () else
      (* TODO: Implement the movement logic *)
  wait_until_q_pressed ()
  

(* Helper functions *)

let fill_poly_color ?color:(color = Graphics.black) poly_list = 
  set_color color;
  fill_poly poly_list

let print_tuple (x,y) =
    Printf.printf "(%d, %d)\n" x y 


(* Top-level funciton *)
(* let render_games_eg2 (input_path: string) (output_path : string): unit =  *)
let render_games_eg2 (input_path: string) (output_path : string) = 

  let get_abs (ox,oy) t_width (x,y) = 
    ox + t_width * x, oy + t_width * y

  in let draw_board r =
    let wrapper_dim = (800, 800) in
    let board_dim = (600, 600) in
    open_graph @@ Printf.sprintf " %dx%d" (fst wrapper_dim) (snd wrapper_dim);
    let lbc_board = ((fst wrapper_dim - fst board_dim) / 2, 
                    (snd wrapper_dim - snd board_dim) / 2) in
    set_color (rgb 255 255 204);
    fill_rect (fst lbc_board) (snd lbc_board) (fst board_dim) (snd board_dim);

    let tile_width = (fst board_dim) / (Array.length r.map) in
    lbc_board, tile_width
    
  
  in let draw_room r lbc_board tile_width =
    (* Fill the room *)  
    let room_int_pairs_array_abs = get_edges_no_shift r |> list_to_array 
        |> Array.map (get_abs lbc_board tile_width) in
    fill_poly_color ~color:(Graphics.yellow) room_int_pairs_array_abs;

    (* draw the lattices
    let all_tiles = get_all_tiles_no_shift_2 r |> list_to_array in
    set_color Graphics.black;
    for i = 0 to Array.length all_tiles - 1 do
      let (x,y) = all_tiles.(i) in 
      let (final_x, final_y) = get_abs lbc_board tile_width (x,y) in
      draw_rect final_x final_y tile_width tile_width
    done *)

  (* TODO *)
  in let draw_clean_boundary = ()
  in let draw_dirty_boundary = () 

  in let display_vroomba lbc_board tile_width (x,y) =
    set_color Graphics.green;
    fill_rect x y tile_width tile_width 

  in let sol_list = ref []
  in let save_moves m_list = 
    String.concat "" (List.rev m_list)

  in let output_sol_list output_path sol_list = 
    write_strings_to_file output_path sol_list 

  in let move_and_clean room state curr dir = 
  (*clean neighbors*)
  let clean_the_region curr =
      (* Check the eight neighbors *)
      let neighbors = get_eight_neighbors curr in
      List.iter (fun coor -> 
                if exist_in_room room coor
                then 
                  (if reachable room curr coor
                  then clean_a_tile state coor)
                )
                neighbors
  in
    (*move current point in dir*)
    let coor = move_in_dir curr dir in 
    
    (* clean the next point *)
    clean_a_tile state coor;
    clean_the_region coor;
    !(state.dirty_tiles) = 0

  in let play r = 
    (* Initialize board and room rendering. 
       Initialize state.
       Display Vroomba at the initial tile *)
    let (lbc_board, tile_width) = draw_board r in
    draw_room r lbc_board tile_width;
    
    let state = initiate_state r in 
    let starting_coor = !(state.current) |> coor_to_map_index r |>
        get_abs lbc_board tile_width in
    display_vroomba lbc_board tile_width starting_coor;

    (* Ask for user input. If the move is valid, update the display 
       of the Vroomba tile and neighbouring tiles that have been cleaned. 
       Record the move. Check whether the room is finished, and if it is,
       check for the user input to proceed with the next room.

       If the move isn't valid, there is no display update. Don't record 
       the move. *)

    (*  *)

    let moves = ref [] in 
    wait_until_q_pressed ()

  in let poly_list = file_to_polygons input_path 
  in let room_list = List.map polygon_to_room_2 poly_list |> list_to_array
  in Array.iter play room_list;
  room_list.(1)

let try_eg_2 () ?file:(file = "basic") =
  let f = BinaryEncodings.find_file "resources/" ^ file ^ ".txt" in 
  let r = render_games_eg2 f "" in 
  r