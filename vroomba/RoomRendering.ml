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
let rec wait_until_q_pressed _ =
  let event = wait_next_event [Key_pressed] in
  if event.key == 'q' 
  then close_graph ()
  else begin
      (* TODO: Implement the movement logic *)
      wait_until_q_pressed ()
    end

(* Helper functions *)

let fill_poly_color ?color:(color = Graphics.black) poly_list = 
  set_color color;
  fill_poly poly_list

let print_tuple (x,y) =
    Printf.printf "(%d, %d)\n" x y 


(* Top-level funciton *)
(* let render_games_eg2 (input_path: string) (output_path : string): unit =  *)
let render_games_eg2 (input_path: string) (output_path : string) = 
  (* open_graph " 1800x1600"; *)
  
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
    (* let room_int_pairs_array_abs = !(r.edges) |> list_to_array |> Array.map (get_abs 
        lbc_board tile_width) in *)
    
    let room_int_pairs_array_abs = get_edges_no_shift r |> list_to_array 
        |> Array.map (get_abs lbc_board tile_width) in
    fill_poly_color ~color:(Graphics.yellow) room_int_pairs_array_abs;

    (* draw the lattices *)
    let all_tiles = get_all_tiles_no_shift r |> list_to_array in
    set_color Graphics.black;
    for i = 0 to Array.length all_tiles - 1 do
      let (x,y) = all_tiles.(i) in 
      let (final_x, final_y) = get_abs lbc_board tile_width (x,y) in
      draw_rect final_x final_y tile_width tile_width
    done

  (* TODO *)
  in let draw_clean_boundary = ()
  in let draw_dirty_boundary = () 
  in let draw_vroomba boundary = ()

  in let display_vroomba r lbc_board tile_width (x,y) =
    set_color Graphics.green;
    fill_rect x y tile_width tile_width 


  in let play r = 
    let (lbc_board, tile_width) = draw_board r in
    draw_room r lbc_board tile_width;
    (* TODO: Functions that need interfacing *)
    let state = init_state r 
    and starting_coor = (100, 100) in
    display_vroomba;
    wait_until_q_pressed ()

  in let poly_list = file_to_polygons input_path 
  in let room_list = List.map polygon_to_room poly_list |> list_to_array
  in Array.iter play room_list;
  room_list.(1)

let try_eg_2 () ?file:(file = "basic") =
  let f = BinaryEncodings.find_file "resources/" ^ file ^ ".txt" in 
  let r = render_games_eg2 f "" in 
  r