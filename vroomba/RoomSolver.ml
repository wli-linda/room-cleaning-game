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
open RoomChecker
open RoomGenerator

(*********************************************)
(*              Room solver                  *)
(*********************************************)

(* This is a complex task. Feel free to introduce whatever functions
   and data types you consider necessary, and also rely on data
   structures and algorithms from the lectures (see folder `lib` of
   this project). *)

(* Solve the room and produce the list of moves. *)
(* Make use of RoomChecker.state state type internally in your solver *)
let solve_room (r: room) : move list = 
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
