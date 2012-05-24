open ELF
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
  module ELF_Identification =
  struct 
    open ELF_Identification

    type elf_identification =
        { ei_class      : ei_class
        ; ei_data       : ei_data
        ; ei_version    : ei_version
        ; ei_osabi      : ei_osabi
        ; ei_abiversion : ei_abiversion
        }

    let read (bs: bitstring): elf_identification =
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

    let write (ei: elf_identification): bitstring =
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

    let to_string (ei: elf_identification): string =
      Printf.sprintf
        "{ %s\n; %s\n; %s\n; %s\n; %s\n}"
        (string_of_ei_class   ei.ei_class)
        (string_of_ei_data    ei.ei_data)
        (string_of_ei_version ei.ei_version)
        (string_of_ei_osabi   ei.ei_osabi)
        (string_of_int        ei.ei_abiversion)
  end

  module ELF_Ehdr =
  struct
    open ELF_Ehdr
    open ELF_Identification

    type et =
      | ET_NONE
      | ET_REL
      | ET_EXEC
      | ET_DYN
      | ET_CORE
      | ET_OS     of int
      | ET_PROC   of int
      | ET_CUSTOM of int

    let (read_et, write_et) =
      let (read_et, write_et) = mk_rw
        [ (0, ET_NONE)
        ; (1, ET_REL)
        ; (2, ET_EXEC)
        ; (3, ET_DYN)
        ; (4, ET_CORE)
        ]
      in
      (
        (fun x ->
          if 0xFE00 <= x && x <= 0xFEFF
          then ET_OS(x)
          else if 0xFF00 <= x && x <= 0xFFFF
          then ET_PROC(x)
          else
            try read_et x
            with Not_found -> ET_CUSTOM(x)
        ),
        (function
        | ET_OS(x)     -> x
        | ET_PROC(x)   -> x
        | ET_CUSTOM(x) -> x
        | x            -> write_et x
        )
      )

  type elf_ehdr =
      { e_ident     : elf_identification
      ; e_type      : et
      ; e_machine   : em
      ; e_version   : ev
      ; e_entry     : int64
      ; e_phoff     : int64
      ; e_shoff     : int64
      ; e_flags     : bitstring
      ; e_ehsize    : int
      ; e_phentsize : int
      ; e_phnum     : int
      ; e_shentsize : int
      ; e_shnum     : int
      ; e_shstrndx  : int
      }
  end
end
