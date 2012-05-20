open ELF_Identification
open Library

let infer_elfclass (bs: bitstring): ei_class =
  bitmatch bs with
  | { ei_class : 8 : int, offset(32), bind(read_ei_class ei_class)
    } -> ei_class

module type ELFCLASS =
sig
  (* ELF fields *)
  type elf_identification
  val read_elf_identification: bitstring -> elf_identification
  val write_elf_identification: elf_identification -> bitstring
  val string_of_elf_identification: elf_identification -> string
end
