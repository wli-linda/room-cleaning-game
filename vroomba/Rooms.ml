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

open ArrayUtil
open Polygons

(*********************************************)
(*         Representation of Rooms           *)
(*********************************************)

(* TODO: provide your implementation of internal room data type *)
(* It should describe the room conveniently for solving. *)
type room = unit

(*  Read a polygon from a string of coordinates as in resources/basic.txt  *)
(*  A string can be ill-formed! *)
let string_to_polygon (s : string) : polygon option = 
  error "Implement me"

(*  Read all polygons from a file  *)
let file_to_polygons (path: string) : polygon list =
  error "Implement me"

let polygon_to_string (p: polygon) : string = 
  error "Implement me"

let write_polygons_to_file (ps: polygon list) (path: string) : unit =
  error "Implement me"

(*********************************************)
(*           Rooms and polygons              *)
(*********************************************)
  
(*  Convert a polygon to a room data type  *)
let polygon_to_room (p: polygon) : room = 
  error "Implement me"

(*  Convert a room to a list of polygon coordinates  *)
let room_to_polygon (r: room) : polygon = 
  error "Implement me"


