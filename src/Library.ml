type bitstring = Bitstring.bitstring

let mk_rw l =
  (
    (fun x -> snd (List.find (fun (k, _) -> k = x) l)),
    (fun x -> fst (List.find (fun (_, k) -> k = x) l))
  )

(*module IntMap = Map.Make(struct type t = int let compare = compare end)*)
module StringMap = Map.Make (String)

let rec is_zeros (bs: bitstring): int -> bool = function
| 0 -> true
| bitsize when bitsize >= 64 ->
    (
      bitmatch bs with
      | { 0L : 64 : int ; rest : -1 : bitstring } ->
          is_zeros rest (bitsize - 64)
      | { _ } -> false
    )
| bitsize ->
    (
      bitmatch bs with
      | { 0L : bitsize : int } -> true
      | { _ } -> false
    )

(*
let is_some: 'a option -> bool = function
| Some(_) -> true
| None    -> false

let from_some: 'a option -> 'a = function
| Some(x) -> x
| None    -> raise Not_found

let filter_some (l: 'a option list): 'a list =
  List.(map from_some (filter is_some l))

type 'a or_err =
  | OK of 'a
  | ERR of string

let is_ok: 'a or_err -> bool = function
| OK(_) -> true
| ERR(_) -> false

let is_err x = not (is_ok x)

let from_ok: 'a or_err -> 'a = function
| OK(x) -> x
| ERR(_) -> assert false

let from_err: 'a or_err -> string = function
| OK(_) -> assert false
| ERR(s) -> s

let filter_ok (l: 'a or_err list): 'a list =
  List.(map from_ok (filter is_ok l))

let filter_err (l: 'a or_err list): string list =
  List.(map from_err (filter is_err l))

external id : 'a -> 'a = "%identity"
*)
(** Checks for existence of an array element satisfying a condition, and returns
    its index if it exists.
*)
let array_exists (cond: 'a -> bool) (arr: 'a array): int option =
  let rec array_exists_aux ndx =
    if ndx < 0
    then None
    else if cond arr.(ndx)
    then Some ndx
    else array_exists_aux (ndx - 1)
  in array_exists_aux (Array.length arr - 1)

let string_of_array string_of_elt sep a =
  let contents =
    (fst
       (Array.fold_left
          (fun accu elt ->
            let (str, ndx) = accu in
            (str ^ (if ndx > 0 then sep else "") ^ string_of_int ndx ^ ": " ^
               string_of_elt elt, ndx + 1)
          )
          ("", 0) a
       )
    )
  in "[\n" ^ contents ^ "\n]"
(*
let string_of_list string_of_elt sep l =
  String.concat sep (List.map string_of_elt l)
*)
let string_of_bitstring bs =
  let rec string_of_bitset_aux bs =
    bitmatch bs with
    | { bit  : 1  : int       ;
        rest : -1 : bitstring } ->
        (if bit then "1" else "0") ^ (string_of_bitset_aux rest)
    | { _ } -> ""
  in string_of_bitset_aux bs

(* To print addresses/offsets *)
let string_of_int32_x = Printf.sprintf "0x%08lx"
(* To print counts/indices *)
let string_of_int32_d = Int32.to_string

let sorted_lookup (compare: 'a -> 'b -> int) (arr: 'a array) (v: 'b): 'a option =
  let rec sorted_lookup_aux (i_from: int) (i_to: int): 'a option =
    if i_from > i_to
    then None
    else
      let i_mid = (i_from + i_to) / 2 in
      let comp = compare arr.(i_mid) v in
      if comp < 0 (* v_mid < v *)
      then sorted_lookup_aux (i_mid + 1) i_to
      else if comp > 0
      then sorted_lookup_aux i_from (i_mid - 1)
      else Some(arr.(i_mid))
  in sorted_lookup_aux 0 (Array.length arr - 1)
