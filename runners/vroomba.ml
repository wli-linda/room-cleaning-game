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

open Printf
open Util

open Rooms
open RoomSolver
open RoomRendering

(*  A placeholder function  *)
let vroom_vroom _ = 
	print_endline "Hello, I'm Vroomba, and I can't wait to be implemented!"

let () =
  if Array.length (Sys.argv) < 4 
  then begin
      print_endline "Not enough input provided";
      print_endline "Possilbe formats: ";
      print_endline "  solve input_file output_file";
      print_endline "  check input_file solutions_file";
      print_endline "  generate num size output_file";
      print_endline "  play input_file output_file";
    end
  else 
    let command = Sys.argv.(1) in
    if command = "play"  
    then
      (* TODO: Implement exception handling if there is no such file *)
      let input_file = Sys.argv.(2) in
      let output_file = Sys.argv.(3) in
      render_games input_file output_file                
    else begin  
        (* TODO: Implement other commands *)
        vroom_vroom (); 
        print_endline "Please, Implement other commands!"
      end
