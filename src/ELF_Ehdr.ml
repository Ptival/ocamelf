open ELF_Identification
open Library

type em =
  | EM_NONE
  | EM_M32
  | EM_SPARC
  | EM_386
  | EM_68K
  | EM_88K
  | EM_860
  | EM_MIPS
  | EM_MIPS_RS4_BE
  | EM_PPC
  | EM_X86_64

let (read_em, write_em) = mk_rw
  [ ( 0, EM_NONE)
  ; ( 1, EM_M32)
  ; ( 2, EM_SPARC)
  ; ( 3, EM_386)
  ; ( 4, EM_68K)
  ; ( 5, EM_88K)
  ; ( 7, EM_860)
  ; ( 8, EM_MIPS)
  ; (10, EM_MIPS_RS4_BE)
  ; (20, EM_PPC)
  ; (62, EM_X86_64)
  ]

let string_of_em = function
| EM_NONE           -> "EM_NONE"
| EM_M32            -> "EM_M32"
| EM_SPARC          -> "EM_SPARC"
| EM_386            -> "EM_386"
| EM_68K            -> "EM_68K"
| EM_88K            -> "EM_88K"
| EM_860            -> "EM_860"
| EM_MIPS           -> "EM_MIPS"
| EM_MIPS_RS4_BE    -> "EM_MIPS_RS4_BE"
| EM_PPC            -> "EM_PPC"
| EM_X86_64         -> "EM_X86_64"

type ev =
  | EV_NONE
  | EV_CURRENT

let (read_ev, write_ev) = mk_rw
  [ (0l, EV_NONE)
  ; (1l, EV_CURRENT)
  ]

let string_of_ev = function
| EV_NONE       -> "EV_NONE"
| EV_CURRENT    -> "EV_CURRENT"
