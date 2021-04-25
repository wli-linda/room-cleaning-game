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
type pos =
  | Edge
  | Inner
  | Outer

type room = {
  map : (pos array) array;
  edges : (int * int) list ref
}

let mk_room size =
  let map = Array.make size [||] in
  for i = 0 to size - 1 do
    map.(i) <- Array.make size Outer
  done;
  map.(0).(0) <- Edge;
  { map = map;
    edges = ref [(0, 0)]
  }

(*  Read a polygon from a string of coordinates as in resources/basic.txt  *)
(*  A string can be ill-formed! *)
let string_to_polygon (s : string) : polygon option =
  let extract_number string =
    let len = String.length string in
    let int_offset = int_of_char '0' in
    let is_neg = ref false in
    let num = ref 0 in
    for i = 1 to len - 1 do
      let ch = string.[i] in
      if i = 1 && ch = '(' then ()
      else if (i = 1 && ch = '-') ||
              (i = 2 && ch = '-' && string.[1] = '(')
      then is_neg := true
      else if int_of_char ch >= int_offset &&
              int_of_char ch <= int_offset + 9
      then num := !num * 10 + ((int_of_char ch) - int_offset)
      else if i = len - 1 && ch = ')' then () (* I realize now that I can just
                                               * use string_of_int here instead
                                               * but for another day I guess *)
      else error "Unrecognizable index!"
    done;
    if !is_neg
    then 0 - !num
    else !num
  in  
  let res = ref [] in
  let coords = String.split_on_char ';' s in
  try (
    List.map (fun e -> String.split_on_char ',' e) coords |>
    List.iter (fun ls ->
        if List.length ls = 2
        then begin
          let x = extract_number @@ List.hd ls in
          let y = extract_number @@ List.nth ls 1 in
          res := (x, y) :: !res
        end
        else error "Ill-formed string!");
    Some (polygon_of_int_pairs (List.rev !res)))
  with error -> None

(*  Read all polygons from a file  *)
let file_to_polygons (path: string) : polygon list =
  let ls = ReadingFiles.read_file_to_strings path in
  let res = ref [] in
  List.iter (fun e -> let poly = (string_to_polygon e) in
              if poly = None then ()
              else res := (get_exn poly) :: !res) ls;
  !res

let polygon_to_string (p: polygon) : string =
  let buffer = Buffer.create 1 in
  List.iter (fun e ->
      let x = get_x e in
      let y = get_y e in
      let x_str = string_of_int (int_of_float x) in
      let y_str = string_of_int (int_of_float y) in
      let coord = String.concat "" ["("; x_str; ", "; y_str; "); "] in
      Buffer.add_string buffer coord) p;
  let s = Buffer.contents buffer in
  String.sub s 0 (String.length s - 2)

let write_polygons_to_file (ps: polygon list) (path: string) : unit =
  let res = ref [] in
  List.iter (fun p -> res := (polygon_to_string p) :: !res) ps;
  ReadingFiles.write_strings_to_file path (List.rev !res)

(*********************************************)
(*           Rooms and polygons              *)
(*********************************************)

let fill_edges map ls =
  let fill_edge x1 y1 x2 y2 =
    if not (x1 = 0 && y1 = 0 && x2 = 0 && y2 = 0) &&
       abs (x1 - x2) = 0 && abs (y1 - y2) = 0
    then error "Invalid room!"
    else if abs (x1 - x2) > 0
    then (if x1 > x2
          then (for i = 1 to (x1 - x2 - 1) do
                  map.(x2 + i).(y1) <- Edge
                done)
          else (for i = 1 to (x2 - x1 - 1) do
                  map.(x1 + i).(y1) <- Edge
                done))
    else if abs (y1 - y2) > 0
    then (if y1 > y2
          then (for i = 1 to (y1 - y2 - 1) do
                  map.(x1).(y2 + i) <- Edge
                done)
          else (for i = 1 to (y2 - y1 - 1) do
                  map.(x1).(y1 + i) <- Edge
                done))
  in
  let rec walk ls a =
    let (x1, y1) = a in
    map.(x1).(y1) <- Edge;
    match ls with
    | [] ->
      fill_edge x1 y1 0 0
    | (x2, y2) :: tl ->
      fill_edge x1 y1 x2 y2;
      walk tl (x2, y2)
  in walk ls (0, 0)

let fill_room map =
  error "impl"

(*  Convert a polygon to a room data type  *)
let polygon_to_room (p: polygon) : room = 
  error "Implement me"

(*  Convert a room to a list of polygon coordinates  *)
let room_to_polygon (r: room) : polygon = 
  polygon_of_int_pairs !(r.edges)


