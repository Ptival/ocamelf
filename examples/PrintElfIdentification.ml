open ELF_Identification

let _ =
  let bs = Bitstring.bitstring_of_file Sys.argv.(1) in
  let ei = read_elf_identification bs in
  print_endline (string_of_elf_identification ei)
