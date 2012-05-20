open ELF
open ELF_Identification
open Library

module Make : ELFCLASS =
struct
  type elf_identification =
      { ei_class      : ei_class
      ; ei_data       : ei_data
      ; ei_version    : ei_version
      }

  let read_elf_identification (bs: bitstring): elf_identification =
    bitmatch bs with
    | { 0x7F       : 8
      ; "ELF"         : 24 : string
      ; ei_class      : 8  : int, bind (read_ei_class      ei_class)
      ; ei_data       : 8  : int, bind (read_ei_data       ei_data)
      ; ei_version    : 8  : int, bind (read_ei_version    ei_version)
      ; padding       : 72 : bitstring
      } ->
        assert (is_zeros padding 72);
        { ei_class
        ; ei_data
        ; ei_version
        }

  let write_elf_identification (ei: elf_identification): bitstring =
    BITSTRING
      { 0x7F                           : 8
      ; "ELF"                          : 24 : string
      ; write_ei_class ei.ei_class     : 8
      ; write_ei_data  ei.ei_data      : 8
      ; write_ei_version ei.ei_version : 8
      ; Bitstring.create_bitstring 72  : 72 : bitstring
      }

  let string_of_elf_identification (ei: elf_identification): string =
    Printf.sprintf
      "{ %s\n; %s\n; %s\n}"
      (string_of_ei_class   ei.ei_class)
      (string_of_ei_data    ei.ei_data)
      (string_of_ei_version ei.ei_version)
end
