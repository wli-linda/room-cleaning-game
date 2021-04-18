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

(*********************************************)
(*       Automated generation of rooms       *)
(*********************************************)

(*  size is the maximal span of the room along both X and Y dimensions *)
(*  Example generate_random_room 4 should return a room that fits into a
    4x4 square. *)

let generate_random_room (size : int) : room = 
  error "Implement me"


(* Define what it means to the room to be valid (e.g., no lacunas,
   obstacles, there is a place for initial Vroomba position, etc). *)
let valid_room (r: room) : bool = 
  error "Implement me"

(*********************************************)
(*                     Tests                 *)
(*********************************************)

(*
let%test "Generated room is valid" = 
  let r = generate_random_room 100 in
  valid_room r
*)

(* TODO: add more tests *)
