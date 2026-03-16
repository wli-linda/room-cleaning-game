open Js_of_ocaml
open Js

module Html = Dom_html

type cell = int * int

type game = {
  mutable tiles : cell list;
  mutable cleaned : (cell, bool) Hashtbl.t;
  mutable current : cell;
  mutable moves : Buffer.t;
  mutable scale : int;
}

let game =
  {
    tiles = [];
    cleaned = Hashtbl.create 256;
    current = (0, 0);
    moves = Buffer.create 256;
    scale = 24;
  }

let document = Html.document

let by_id id =
  match Html.getElementById_coerce id Html.CoerceTo.element with
  | None -> failwith ("Missing DOM node: " ^ id)
  | Some e -> e

let as_canvas e = Js.Opt.get (Html.CoerceTo.canvas e) (fun () -> failwith "canvas expected")
let as_button e = Js.Opt.get (Html.CoerceTo.button e) (fun () -> failwith "button expected")
let as_textarea e = Js.Opt.get (Html.CoerceTo.textarea e) (fun () -> failwith "textarea expected")
let as_input e = Js.Opt.get (Html.CoerceTo.input e) (fun () -> failwith "input expected")

let status_el = by_id "status"
let room_input = as_textarea (by_id "room-input")
let load_btn = as_button (by_id "load-room")
let reset_btn = as_button (by_id "reset-room")
let moves_el = by_id "moves"
let rooms_el = as_textarea (by_id "rooms-input")
let load_rooms_btn = as_button (by_id "load-rooms")
let next_room_btn = as_button (by_id "next-room")
let room_idx_el = by_id "room-index"
let scale_input = as_input (by_id "scale")
let canvas = as_canvas (by_id "board")
let ctx =
  Js.Opt.get
    (canvas##getContext (Html._2d_))
    (fun () -> failwith "2d context unavailable")

let room_queue : string array ref = ref [||]
let room_idx = ref 0

let set_text el s = el##.textContent := Js.Opt.return (Js.string s)

let parse_points (s : string) : (int * int) list option =
  let chunks = String.split_on_char ';' s |> List.filter (fun x -> String.trim x <> "") in
  let parse_one part =
    let p = String.trim part in
    let len = String.length p in
    if len < 5 then None
    else
      let inner =
        if p.[0] = '(' && p.[len - 1] = ')' then String.sub p 1 (len - 2)
        else p
      in
      match String.split_on_char ',' inner with
      | [a; b] -> (
          try Some (int_of_string (String.trim a), int_of_string (String.trim b)) with _ -> None)
      | _ -> None
  in
  let rec collect acc = function
    | [] -> Some (List.rev acc)
    | h :: t -> (
        match parse_one h with
        | None -> None
        | Some p -> collect (p :: acc) t)
  in
  collect [] chunks

let bounds points =
  match points with
  | [] -> (0, 0, 0, 0)
  | (x0, y0) :: tl ->
      List.fold_left
        (fun (minx, miny, maxx, maxy) (x, y) ->
          (min minx x, min miny y, max maxx x, max maxy y))
        (x0, y0, x0, y0) tl

let point_in_polygon (x : float) (y : float) (poly : (int * int) list) : bool =
  let rec loop pts inside =
    match pts with
    | [] | [_] -> inside
    | (x1, y1) :: ((x2, y2) :: _ as tl) ->
        let y1f = float_of_int y1 and y2f = float_of_int y2 in
        let x1f = float_of_int x1 and x2f = float_of_int x2 in
        let crosses =
          ((y1f > y) <> (y2f > y))
          && x < ((x2f -. x1f) *. (y -. y1f) /. ((y2f -. y1f) +. 1e-9) +. x1f)
        in
        loop tl (if crosses then not inside else inside)
  in
  match poly with
  | [] -> false
  | _ ->
      let closed = poly @ [List.hd poly] in
      loop closed false

let make_tiles points =
  let minx, miny, maxx, maxy = bounds points in
  let acc = ref [] in
  for x = minx to maxx - 1 do
    for y = miny to maxy - 1 do
      if point_in_polygon (float_of_int x +. 0.5) (float_of_int y +. 0.5) points then
        acc := (x, y) :: !acc
    done
  done;
  List.rev !acc

let reset_cleaned () =
  Hashtbl.clear game.cleaned;
  List.iter (fun c -> Hashtbl.replace game.cleaned c false) game.tiles;
  match game.tiles with
  | [] -> ()
  | h :: _ ->
      game.current <- h;
      Hashtbl.replace game.cleaned h true;
      Buffer.clear game.moves

let remaining () =
  List.fold_left
    (fun n c -> if Hashtbl.find_opt game.cleaned c = Some true then n else n + 1)
    0 game.tiles

let is_tile c = List.exists (( = ) c) game.tiles

let draw () =
  let minx, miny, maxx, maxy = bounds game.tiles in
  let w = max 1 (maxx - minx) in
  let h = max 1 (maxy - miny) in
  canvas##.width := w * game.scale;
  canvas##.height := h * game.scale;

  ctx##.fillStyle := Js.string "#10131a";
  ctx##fillRect 0. 0. (float_of_int canvas##.width) (float_of_int canvas##.height);

  List.iter
    (fun (x, y) ->
      let px = (x - minx) * game.scale in
      let py = (h - 1 - (y - miny)) * game.scale in
      let clean = Hashtbl.find_opt game.cleaned (x, y) = Some true in
      ctx##.fillStyle := Js.string (if clean then "#7dd3fc" else "#374151");
      ctx##fillRect (float_of_int px) (float_of_int py) (float_of_int game.scale) (float_of_int game.scale);
      ctx##.strokeStyle := Js.string "#1f2937";
      ctx##strokeRect (float_of_int px) (float_of_int py) (float_of_int game.scale) (float_of_int game.scale))
    game.tiles;

  let cx, cy = game.current in
  let px = (cx - minx) * game.scale in
  let py = (h - 1 - (cy - miny)) * game.scale in
  ctx##.fillStyle := Js.string "#22c55e";
  ctx##fillRect (float_of_int px +. 2.) (float_of_int py +. 2.) (float_of_int (game.scale - 4))
    (float_of_int (game.scale - 4));

  set_text moves_el (Buffer.contents game.moves);
  set_text status_el (Printf.sprintf "Tiles left: %d" (remaining ()))

let load_room_string s =
  match parse_points s with
  | None -> set_text status_el "Could not parse room string"
  | Some pts ->
      let tiles = make_tiles pts in
      if tiles = [] then set_text status_el "Room has no interior tiles"
      else (
        game.tiles <- tiles;
        reset_cleaned ();
        draw ())

let move_by (dx, dy) key =
  let x, y = game.current in
  let n = (x + dx, y + dy) in
  if is_tile n then (
    game.current <- n;
    Hashtbl.replace game.cleaned n true;
    Buffer.add_char game.moves key;
    draw ())

let on_keydown ev =
  let k = Js.to_string ev##.key in
  (match String.lowercase_ascii k with
  | "w" -> move_by (0, 1) 'W'
  | "a" -> move_by (-1, 0) 'A'
  | "s" -> move_by (0, -1) 'S'
  | "d" -> move_by (1, 0) 'D'
  | _ -> ());
  Js._true

let load_queue () =
  let lines =
    Js.to_string rooms_el##.value |> String.split_on_char '\n'
    |> List.map String.trim |> List.filter (fun s -> s <> "") |> Array.of_list
  in
  room_queue := lines;
  room_idx := 0;
  if Array.length lines > 0 then load_room_string lines.(0);
  set_text room_idx_el (Printf.sprintf "%d / %d" (if Array.length lines=0 then 0 else 1) (Array.length lines))

let next_room () =
  if Array.length !room_queue = 0 then set_text status_el "No queued rooms"
  else (
    room_idx := !room_idx + 1;
    if !room_idx >= Array.length !room_queue then room_idx := Array.length !room_queue - 1
    else ();
    load_room_string (!room_queue).(!room_idx);
    set_text room_idx_el (Printf.sprintf "%d / %d" (!room_idx + 1) (Array.length !room_queue)))

let () =
  Html.addEventListener document Html.Event.keydown (Html.handler (fun ev -> on_keydown ev)) Js._false
  |> ignore;

  load_btn##.onclick :=
    Html.handler (fun _ ->
        load_room_string (Js.to_string room_input##.value);
        Js._false);

  reset_btn##.onclick :=
    Html.handler (fun _ ->
        reset_cleaned ();
        draw ();
        Js._false);

  load_rooms_btn##.onclick := Html.handler (fun _ -> load_queue (); Js._false);
  next_room_btn##.onclick := Html.handler (fun _ -> next_room (); Js._false);

  scale_input##.onchange :=
    Html.handler (fun _ ->
        (try game.scale <- int_of_string (Js.to_string scale_input##.value) with _ -> ());
        if game.scale < 8 then game.scale <- 8;
        if game.scale > 64 then game.scale <- 64;
        draw ();
        Js._false);

  room_input##.value := Js.string "(0,0); (8,0); (8,6); (5,6); (5,3); (3,3); (3,6); (0,6)";
  rooms_el##.value := Js.string "";
  load_room_string (Js.to_string room_input##.value)
