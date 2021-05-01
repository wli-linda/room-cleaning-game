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
open Rooms
open RoomChecker
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
  then exit 0
  else begin
      (* TODO: Implement the movement logic *)
      wait_until_q_pressed ()
    end

(*  Top-level file for starting the game  *)
let render_games_eg (input_path: string) (output_path : string): unit = 
  open_graph " 800x600";
  (* Example room, resized and shifted to look nice *)
  let p = [(0, 0); (2, 0); (2, -3); (3, -3); (3, -5); (4, -5); (4, -6); (1, -6); (1, -2); (-2, -2); (-2, -6); (-1, -6); (-1, -7); (-3, -7); (-3, -1); (-4, -1); (-4, 19); (-16, 19); (-16, -1); (-14, -1); (-14, -3); (-24, -3); (-24, 7); (-30, 7); (-30, -3); (-28, -3); (-28, -24); (-14, -24); (-14, -13); (-10, -13); (-10, -1); (-9, -1); (-9, -7); (-6, -7); (-6, -10); (-1, -10); (-1, -9); (4, -9); (4, -8); (6, -8); (6, -5); (6, -3); (5, -3); (5, 0); (4, 0); (4, 1); (6, 1); (6, -2); (9, -2); (9, -1); (12, -1); (12, 2); (9, 2); (9, 1); (8, 1); (8, 7); (6, 7); (6, 10); (3, 10); (3, 7); (4, 7); (4, 4); (0, 4)] 
          |> polygon_of_int_pairs 
          |> resize_polygon 80.
          |> shift_polygon (-300., -100.)

  in draw_polygon p;
  (* TODO: Implement the rest *)
  wait_until_q_pressed ()


let render_games_eg (input_path: string) (output_path : string): unit = 
  open_graph " 800x600";
  (* Example room, resized and shifted to look nice *)
  let play r =
    wait_until_q_pressed ()
  in let poly_list = file_to_polygons input_path 
  in let room_list = List.map polygon_to_room poly_list 
  in List.iter play room_list


  (* TODO: Implement the rest *)

