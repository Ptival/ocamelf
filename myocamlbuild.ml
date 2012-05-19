open Ocamlbuild_plugin;;
dispatch begin function
| After_rules ->
    (* ocamlfind libraries *)
    flag ["ocaml"; "compile";  "pkg_bitstring"] & S[A"-package"; A"bitstring,bitstring.syntax"; A"-syntax"; A"bitstring.syntax,camlp4o"];
    flag ["ocaml"; "ocamldep";  "pkg_bitstring"] & S[A"-package"; A"bitstring,bitstring.syntax"; A"-syntax"; A"bitstring.syntax,camlp4o"];
    flag ["ocaml"; "link";  "pkg_bitstring"] & S[A"-package"; A"bitstring"]
| _ -> ()
end
