
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

(*  TODO: Implement me!  *)
(* This is a data type, representing the state of the room at a
   certain point of cleaning. It should include the room and keep
   track of Vroomba's current position, parts that are already cleaned
   and that are remaining to be cleaned. Use this data type internally
   in the function `check_solution` *)

type state = unit (* You should change this definition *)

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
       Some !res)
  with error ->
    None
      
(*  Check that the sequence of moves is valid  *)
let check_solution (r: room) (moves: move list) : bool = 
  error "Implement me!"

(*  Top-level validator  *)
let validate r s = 
  match string_to_solution s with
  | None -> false
  | Some moves -> check_solution r moves
 
(* 
let%test _ = 
  let s = "(0, 0); (6, 0); (6, 1); (8, 1); (8, 2); (6, 2); (6, 3); (0, 3)" in
  let room = string_to_polygon s |> get_exn |> polygon_to_room in
  validate room "WDDDDDD" 
*)

(* TODO: Add more tests *)                                                 
