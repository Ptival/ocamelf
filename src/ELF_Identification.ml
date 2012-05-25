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

let bitstring_endian_of_ei_data = function
| ELFDATA2LSB -> Bitstring.LittleEndian
| ELFDATA2MSB -> Bitstring.BigEndian
| _           -> failwith "Unknown endianness"

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
