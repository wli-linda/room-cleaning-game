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
  error "Implement me!"

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
let render_games (input_path: string) (output_path : string): unit = 
  open_graph " 800x600";
  (* Example room, resized and shifted to look nice *)
  let p = [(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)] 
          |> polygon_of_int_pairs 
          |> resize_polygon 80.
          |> shift_polygon (-300., -100.)
  in
  draw_polygon p;
  (* TODO: Implement the rest *)
  wait_until_q_pressed ()
