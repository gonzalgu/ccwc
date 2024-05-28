open Unix


let file_size filename = 
  let stats = Unix.stat filename in 
  stats.st_size

let count_lines filename = 
  let in_channel = open_in filename in
  let rec count acc = 
    try
      let _ = input_line in_channel in 
      count (acc + 1)  
    with 
    | End_of_file -> close_in in_channel; 
    acc
  in 
  count 0


let count_words filename =
    let in_channel = open_in filename in
    let rec count acc =
      try
        let line = input_line in_channel in
        let words = Str.split (Str.regexp "[ \t\n\r\x0c]+") line in
        count (acc + List.length(words))
      with
      | End_of_file -> close_in in_channel; 
      acc
    in
    count 0

let count_characters (filename : string) : int =
  let ic = open_in filename in
  let chars = ref 0 in
    try      
      while true do
        ignore (input_char ic);
        incr chars
      done;
      !chars
    with
    | End_of_file -> close_in ic; !chars
    | e -> close_in ic; raise e    
      
let process_command cmd filename = 
  match cmd with 
  | "-c"  ->  file_size filename
  | "-l" -> count_lines filename
  | "-w" -> count_words filename
  | "-m" -> count_characters filename
  | _ -> -1


let () = 
  let c = Sys.argv.(1) in
  let f = Sys.argv.(2) in
  Printf.printf "%d %s\n" (process_command c f) f

