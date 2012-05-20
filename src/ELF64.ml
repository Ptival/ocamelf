open ELF
open ELF_Identification
open Library

type ei_osabi =
  | ELFOSABI_SYSV
  | ELFOSABI_HPUX
  | ELFOSABI_STANDALONE

let (read_ei_osabi, write_ei_osabi) = mk_rw
  [ (  0, ELFOSABI_SYSV)
  ; (  1, ELFOSABI_HPUX)
  ; (255, ELFOSABI_STANDALONE)
  ]

let string_of_ei_osabi = function
| ELFOSABI_SYSV       -> "ELFOSABI_SYSV"
| ELFOSABI_HPUX       -> "ELFOSABI_HPUX"
| ELFOSABI_STANDALONE -> "ELFOSABI_STANDALONE"

type ei_abiversion = int

module Make : ELFCLASS =
struct
  type elf_identification =
      { ei_class      : ei_class
      ; ei_data       : ei_data
      ; ei_version    : ei_version
      ; ei_osabi      : ei_osabi
      ; ei_abiversion : ei_abiversion
      }

  let read_elf_identification (bs: bitstring): elf_identification =
    bitmatch bs with
    | { 0x7F          : 8
      ; "ELF"         : 24 : string
      ; ei_class      : 8  : int, bind (read_ei_class      ei_class)
      ; ei_data       : 8  : int, bind (read_ei_data       ei_data)
      ; ei_version    : 8  : int, bind (read_ei_version    ei_version)
      ; ei_osabi      : 8  : int, bind (read_ei_osabi      ei_osabi)
      ; ei_abiversion : 8  : int
      ; padding       : 56 : bitstring
      } ->
        assert (is_zeros padding 56);
        { ei_class
        ; ei_data
        ; ei_version
        ; ei_osabi
        ; ei_abiversion
        }

          
  let write_elf_identification (ei: elf_identification): bitstring =
    BITSTRING
      { 0x7F                           : 8
      ; "ELF"                          : 24 : string
      ; write_ei_class   ei.ei_class   : 8
      ; write_ei_data    ei.ei_data    : 8
      ; write_ei_version ei.ei_version : 8
      ; write_ei_osabi   ei.ei_osabi   : 8
      ; ei.ei_abiversion               : 8
      ; Bitstring.create_bitstring 56  : 56 : bitstring
      }

  let string_of_elf_identification (ei: elf_identification): string =
    Printf.sprintf
      "{ %s\n; %s\n; %s\n; %s\n; %s\n}"
      (string_of_ei_class   ei.ei_class)
      (string_of_ei_data    ei.ei_data)
      (string_of_ei_version ei.ei_version)
      (string_of_ei_osabi   ei.ei_osabi)
      (string_of_int        ei.ei_abiversion)
end
