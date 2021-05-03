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

(*  Top-level file for starting the game  *)
let render_games (input_path: string) (output_path : string): unit = 
  open_graph " 800x600";
  (* Example room, resized and shifted to look nice *)
  let p = [(0, 0); (2, 0); (2, -3); (3, -3); (3, -5); (4, -5); (4, -6); (1, -6); (1, -2); (-2, -2); (-2, -6); (-1, -6); (-1, -7); (-3, -7); (-3, -1); (-4, -1); (-4, 19); (-16, 19); (-16, -1); (-14, -1); (-14, -3); (-24, -3); (-24, 7); (-30, 7); (-30, -3); (-28, -3); (-28, -24); (-14, -24); (-14, -13); (-10, -13); (-10, -1); (-9, -1); (-9, -7); (-6, -7); (-6, -10); (-1, -10); (-1, -9); (4, -9); (4, -8); (6, -8); (6, -5); (6, -3); (5, -3); (5, 0); (4, 0); (4, 1); (6, 1); (6, -2); (9, -2); (9, -1); (12, -1); (12, 2); (9, 2); (9, 1); (8, 1); (8, 7); (6, 7); (6, 10); (3, 10); (3, 7); (4, 7); (4, 4); (0, 4)] 
          |> polygon_of_int_pairs 
          |> resize_polygon 80.
          |> shift_polygon (-300., -100.)

  in draw_polygon p;
  (* TODO: Implement the rest *)
  wait_until_q_pressed ()

let fill_poly_color ?color:(color = Graphics.black) poly_list = 
  set_color color;
  fill_poly poly_list

let print_tuple (x,y) =
    Printf.printf "(%d, %d)\n" x y 

let check_square x y arr = 
    let corners = [|(x, y); (x, y+1); (x+1, y); (x+1, y+1)|] in 
    let inside = ref true and 
        i = ref 0 in 
    while !i < 4 && !inside do
      begin
      try begin
        let (x', y') = corners.(!i) in
        print_tuple (x', y');
        if arr.(x').(y') == Outer then inside := false end
      with _ -> ()
      end;
      i := !i + 1
    done;
    !inside

(* let load_vroomba_picture path =  *)

  
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


  in let draw_vroomba r lbc_board tile_width (x,y) =
    set_color Graphics.green;
    fill_rect x y tile_width tile_width 

  in let play r = 
    let (lbc_board, tile_width) = draw_board r in
    draw_room r lbc_board tile_width;
    wait_until_q_pressed ()

  in let poly_list = file_to_polygons input_path 
  in let room_list = List.map polygon_to_room_2 poly_list |> list_to_array
  in Array.iter play room_list;
  room_list.(1)


(* let render_games_eg (input_path: string) (output_path : string): unit = 
  open_graph " 800x600";
  (* Check whether the block with (x,y) as the left bottom corner is 
    inside the room *)
  (* Linda's version: inside_room in room solver  *)
  

  (* Example room, resized and shifted to look nice *)
  let play r =
    (* Use the index as the coordinates first *)
    (* Draw the board *)
    let size = Array.length r.map 
    and scale = 50.0
    in let board_int_pairs = [(0, 0); (0, size); (size, size); (size, 0)] in 

    let board_poly = polygon_of_int_pairs board_int_pairs in 
    let scaled_board_poly = resize_polygon scale board_poly in
    draw_polygon ~color:(Graphics.blue) scaled_board_poly;

    (* Can print a short instruction *)

    (* Fill the room with Ocaml fill_poly and then draw the room boundary 
        with draw_polygon *)
    (* let room_poly = !(r.edges) |> polygon_of_int_pairs in  *)
    let room_int_pairs = !(r.edges) in 
    let room_poly = polygon_of_int_pairs room_int_pairs in
    let scaled_room_poly = resize_polygon scale room_poly in 
    let scaled_room_poly_abs = shift_polygon (400.0, 300.0) scaled_room_poly in
    (* let scaled_room_int_pairs = polygon_to_int_pairs scaled_room_poly in  *)
    let scaled_room_int_pairs_abs = polygon_to_int_pairs scaled_room_poly_abs in

    print_string @@ "scaled board_poly: " ^ (polygon_to_string scaled_board_poly) ^ "\n";
    print_string @@ "scaled room_poly_abs: " ^ (polygon_to_string scaled_room_poly_abs) ^ "\n"; 
    fill_poly_color ~color:(Graphics.yellow) @@ list_to_array scaled_room_int_pairs_abs;
    draw_polygon ~color:(Graphics.red) scaled_room_poly;
    draw_point ~color:(Graphics.green) @@ coor_to_point (605, 5);
    wait_until_q_pressed ()

    (* Draw the lines separating all the lattices *)

  in let poly_list = file_to_polygons input_path 
  (* in let room_list = List.map polygon_to_room poly_list 
  in List.iter play room_list *)
  in let room_list = List.map polygon_to_room poly_list 
  in List.iter play room_list

let try_eg () =
  let f = BinaryEncodings.find_file "resources/basic.txt" 
  in render_games_eg f ""
  (* TODO: Implement the rest *) *)

let try_eg_2 () ?file:(file = "basic") =
  let f = BinaryEncodings.find_file "resources/" ^ file ^ ".txt" in 
  let r = render_games_eg2 f "" in 
  r