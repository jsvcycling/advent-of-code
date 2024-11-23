open Core

let () =
  let lines = In_channel.read_lines "input/01-real.txt" in
  let make_result digits =
    String.of_char_list [ List.hd_exn digits; List.last_exn digits ]
  in
  let process_line line =
    line |> String.to_list |> List.filter ~f:Char.is_digit |> make_result |> Int.of_string
  in
  let values = List.map lines ~f:process_line in
  let result = List.fold values ~init:0 ~f:( + ) in
  Out_channel.printf "Part 1: %d\n" result
;;

let max_digit_len = 5

let as_digit s =
  match s with
  | "zero" | "0" -> Some '0'
  | "one" | "1" -> Some '1'
  | "two" | "2" -> Some '2'
  | "three" | "3" -> Some '3'
  | "four" | "4" -> Some '4'
  | "five" | "5" -> Some '5'
  | "six" | "6" -> Some '6'
  | "seven" | "7" -> Some '7'
  | "eight" | "8" -> Some '8'
  | "nine" | "9" -> Some '9'
  | _ -> None
;;

(* This is super ugly but it works. Should probably be refactored in a more
   OCaml-esque style. *)
let process_line s =
  let numbers = ref [] in
  for pos = 0 to String.length s do
    for len = 1 to max_digit_len do
      let test_len = Int.clamp_exn len ~min:0 ~max:(String.length s - pos) in
      let str_bytes = Bytes.of_string s in
      let sub = Substring.create ~pos ~len:test_len str_bytes in
      match as_digit (Substring.to_string sub) with
      | Some v -> numbers := v :: !numbers
      | None -> ()
    done
  done;
  List.rev !numbers
;;

let () =
  let lines = In_channel.read_lines "input/01-real.txt" in
  let result =
    lines
    |> List.map ~f:process_line
    |> List.map ~f:(fun l ->
      String.of_char_list [ List.hd_exn l; List.last_exn l ] |> Int.of_string)
    |> List.fold ~init:0 ~f:( + )
  in
  Out_channel.printf "Part 2: %d\n" result
;;
