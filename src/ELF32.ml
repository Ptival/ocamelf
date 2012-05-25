open ELF
open Library

module Make : ELFCLASS =
struct
  module Identification =
  struct
    open ELF_Identification

    type elf_identification =
        { ei_class      : ei_class
        ; ei_data       : ei_data
        ; ei_version    : ei_version
        }

    let read (bs: bitstring): elf_identification =
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
            
    let write (ei: elf_identification): bitstring =
      BITSTRING
        { 0x7F                           : 8
        ; "ELF"                          : 24 : string
        ; write_ei_class ei.ei_class     : 8
        ; write_ei_data  ei.ei_data      : 8
        ; write_ei_version ei.ei_version : 8
        ; Bitstring.create_bitstring 72  : 72 : bitstring
        }

    let to_string (ei: elf_identification): string =
      Printf.sprintf
        "
{ ei_class   = %s
; ei_data    = %s
; ei_version = %s
}"
        (string_of_ei_class   ei.ei_class)
        (string_of_ei_data    ei.ei_data)
        (string_of_ei_version ei.ei_version)
  end

  module Ehdr =
  struct
    open ELF_Ehdr
    open ELF_Identification
    open Identification

    type et =
      | ET_NONE
      | ET_REL
      | ET_EXEC
      | ET_DYN
      | ET_CORE
      | ET_PROC  of int
      | ET_OTHER of int

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
          if 0xFF00 <= x && x <= 0xFFFF
          then ET_PROC(x)
          else
            try read_et x
            with Not_found -> ET_OTHER(x)
        ),
        (function
        | ET_PROC(x)  -> x
        | ET_OTHER(x) -> x
        | x           -> write_et x
        )
      )

    let string_of_et = function
    | ET_NONE     -> "ET_NONE"
    | ET_REL      -> "ET_REL"
    | ET_EXEC     -> "ET_EXEC"
    | ET_DYN      -> "ET_DYN"
    | ET_CORE     -> "ET_CORE"
    | ET_PROC(x)  -> "ET_PROC(" ^ string_of_int x ^ ")"
    | ET_OTHER(x) -> "ET_OTHER(" ^ string_of_int x ^ ")"

    type elf_ehdr =
        { e_ident     : elf_identification
        ; e_type      : et
        ; e_machine   : em
        ; e_version   : ev
        ; e_entry     : int32
        ; e_phoff     : int32
        ; e_shoff     : int32
        ; e_flags     : bitstring
        ; e_ehsize    : int
        ; e_phentsize : int
        ; e_phnum     : int
        ; e_shentsize : int
        ; e_shnum     : int
        ; e_shstrndx  : int
        }

    let read (e_ident: elf_identification) (bs: bitstring): elf_ehdr =
      let endian = bitstring_endian_of_ei_data e_ident.ei_data in
      bitmatch Bitstring.dropbits 128 bs with
        { e_type      : 16 : int, endian(endian), bind (read_et e_type)
        ; e_machine   : 16 : int, endian(endian), bind (read_em e_machine)
        ; e_version   : 32 : int, endian(endian), bind (read_ev e_version)
        ; e_entry     : 32 : int, endian(endian)
        ; e_phoff     : 32 : int, endian(endian)
        ; e_shoff     : 32 : int, endian(endian)
        ; e_flags     : 32 : bitstring
        ; e_ehsize    : 16 : int, endian(endian)
        ; e_phentsize : 16 : int, endian(endian)
        ; e_phnum     : 16 : int, endian(endian)
        ; e_shentsize : 16 : int, endian(endian)
        ; e_shnum     : 16 : int, endian(endian)
        ; e_shstrndx  : 16 : int, endian(endian)
        } ->
          { e_ident
          ; e_type
          ; e_machine
          ; e_version
          ; e_entry
          ; e_phoff
          ; e_shoff
          ; e_flags
          ; e_ehsize
          ; e_phentsize
          ; e_phnum
          ; e_shentsize
          ; e_shnum
          ; e_shstrndx
          }

    let to_string eh =
      Printf.sprintf
        "
{ e_ident     = %s
; e_type      = %s
; e_machine   = %s
; e_version   = %s
; e_entry     = %s
; e_phoff     = %s
; e_shoff     = %s
; e_flags     = %s
; e_ehsize    = %s
; e_phentsize = %s
; e_phnum     = %s
; e_shentsize = %s
; e_shnum     = %s
; e_shstrndx  = %s
}"
        (Identification.to_string eh.e_ident)
        (string_of_et             eh.e_type)
        (string_of_em             eh.e_machine)
        (string_of_ev             eh.e_version)
        (string_of_int32_x        eh.e_entry)
        (string_of_int32_x        eh.e_phoff)
        (string_of_int32_x        eh.e_shoff)
        (string_of_bitstring      eh.e_flags)
        (string_of_int            eh.e_ehsize)
        (string_of_int            eh.e_phentsize)
        (string_of_int            eh.e_phnum)
        (string_of_int            eh.e_shentsize)
        (string_of_int            eh.e_shnum)
        (string_of_int            eh.e_shstrndx)

  end
end
