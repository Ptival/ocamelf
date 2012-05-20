open Library

type ei_class =
  | ELFCLASSNONE
  | ELFCLASS32
  | ELFCLASS64

let (read_ei_class, write_ei_class) = mk_rw
  [ (0, ELFCLASSNONE)
  ; (1, ELFCLASS32)
  ; (2, ELFCLASS64)
  ]

let string_of_ei_class = function
| ELFCLASSNONE -> "ELFCLASSNONE"
| ELFCLASS32   -> "ELFCLASS32"
| ELFCLASS64   -> "ELFCLASS64"

type ei_data =
  | ELFDATANONE
  | ELFDATA2LSB
  | ELFDATA2MSB

let (read_ei_data, write_ei_data) = mk_rw
  [ (0, ELFDATANONE)
  ; (1, ELFDATA2LSB)
  ; (2, ELFDATA2MSB)
  ]

let string_of_ei_data = function
| ELFDATANONE -> "ELFDATANONE"
| ELFDATA2LSB -> "ELFDATA2LSB"
| ELFDATA2MSB -> "ELFDATA2MSB"

type ei_version =
  | EV_NONE
  | EV_CURRENT

let (read_ei_version, write_ei_version) = mk_rw
  [ (0, EV_NONE)
  ; (1, EV_CURRENT)
  ]

let string_of_ei_version = function
| EV_NONE    -> "EV_NONE"
| EV_CURRENT -> "EV_CURRENT"

type elf_identification =
    { ei_class   : ei_class
    ; ei_data    : ei_data
    ; ei_version : ei_version
    }

let read_elf_identification (bs: bitstring): elf_identification =
  bitmatch bs with
  | { 0x7F       : 8
    ; "ELF"      : 24 : string
    ; ei_class   : 8  : int, bind (read_ei_class   ei_class)
    ; ei_data    : 8  : int, bind (read_ei_data    ei_data)
    ; ei_version : 8  : int, bind (read_ei_version ei_version)
    ; padding    : 72 : bitstring
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
