open Core

let () =
  let args = Sys.get_argv () in
  try
    let day = Array.get args 1 in
    match day with
    | "day01" ->
      let lines = In_channel.read_lines "input/01-real.txt" in
      Day_01.run lines
    | _ -> print_endline ("Unexpected argument \"" ^ day ^ "\".")
  with
  | Invalid_argument _ -> print_endline "Please provide a command."
;;
