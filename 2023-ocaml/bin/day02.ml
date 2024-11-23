open Core

let parse_die s =
  let s = String.strip s in
  let count, color = Scanf.sscanf s "%d %s" (fun a b -> a, b) in
  match color with
  | "red" -> count, 0, 0
  | "green" -> 0, count, 0
  | "blue" -> 0, 0, count
  | _ -> failwith color
;;

let sum_tuple3 (a1, b1, c1) (a2, b2, c2) = a1 + a2, b1 + b2, c1 + c2

let parse_roll f s =
  String.split s ~on:','
  |> List.map ~f:String.strip
  |> List.map ~f:parse_die
  |> List.fold ~init:(0, 0, 0) ~f
;;

let is_invalid_game (r, g, b) = r > 12 || g > 13 || b > 14

let parse_game s =
  let game_id, rolls = Scanf.sscanf s "Game %d: %[^\n]" (fun a b -> a, b) in
  let invalid_rolls =
    String.split rolls ~on:';'
    |> List.map ~f:(parse_roll sum_tuple3)
    |> List.filter ~f:is_invalid_game
  in
  match List.length invalid_rolls with
  | 0 -> game_id
  | _ -> 0
;;

let () =
  let result =
    In_channel.read_lines "input/02-real.txt"
    |> List.map ~f:parse_game
    |> List.fold ~init:0 ~f:( + )
  in
  Out_channel.printf "Part 1: %d\n" result
;;

(* --- *)

let max_tuple3 (r1, g1, b1) (r2, g2, b2) = Int.max r1 r2, Int.max g1 g2, Int.max b1 b2

let parse_game2 s =
  let roll_sets = Scanf.sscanf s "Game %d: %[^\n]" (fun _ b -> b) in
  let dies = String.split_on_chars roll_sets ~on:[ ','; ';' ] in
  let r, g, b = List.map dies ~f:parse_die |> List.fold ~init:(0, 0, 0) ~f:max_tuple3 in
  r * g * b
;;

let () =
  let result =
    In_channel.read_lines "input/02-real.txt"
    |> List.map ~f:parse_game2
    |> List.fold ~init:0 ~f:( + )
  in
  Out_channel.printf "Part 2: %d\n" result
;;
