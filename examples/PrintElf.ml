open ELF_parsers
open ELF_printers

let _ =
  if Array.length Sys.argv <> 2
  then Printf.printf "Usage: %s <some-elf>\n" Sys.argv.(0)
  else
    let elf = read_elf Sys.argv.(1) in
    print_endline (string_of_elf elf)
