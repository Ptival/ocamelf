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
  let phdr_array = ELF.Phdr.read eh bs in
  let sym_array = ELF.Sym.read eh shdr_array bs in
  print_endline (ELF.Ehdr.to_string eh);
  Array.iter (fun shdr -> print_endline (ELF.Shdr.to_string shdr)) shdr_array;
  Array.iter (fun phdr -> print_endline (ELF.Phdr.to_string phdr)) phdr_array;
  Array.iter (fun sym -> print_endline (ELF.Sym.to_string sym)) sym_array
