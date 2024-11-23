open Core

let () =
  let args = Sys.get_argv () in
  try
    let day = Array.get args 1 in
    match day with
    | "day01" -> failwith "Day 01 has not been implemented yet."
    | _ -> print_endline ("Unexpected argument \"" ^ day ^ "\".")
  with
  | Invalid_argument _ -> print_endline "Please provide a command."
;;
