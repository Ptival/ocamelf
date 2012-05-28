open ELF
open ELF_Identification

let _ =
  let bs = Bitstring.bitstring_of_file Sys.argv.(1) in
  let elfclass = infer_elfclass bs in
  let module ELF = (val (
    match elfclass with
    | ELFCLASS32   -> (module ELF32.Make : ELFCLASS)
    | ELFCLASS64   -> (module ELF64.Make : ELFCLASS)
    | ELFCLASSNONE -> failwith "Unsupported ELF class"
  ) : ELFCLASS)
  in
  let ei = ELF.Identification.read bs in
  let eh = ELF.Ehdr.read ei bs in
  let shdr_array = ELF.Shdr.read eh bs in
  print_endline (ELF.Ehdr.to_string eh);
  Array.iter (fun shdr -> print_endline (ELF.Shdr.to_string shdr)) shdr_array
