OCAMLBUILD=ocamlbuild
OCB_OPTIONS=-I src -I examples -use-ocamlfind

all: printelf

printelf: examples/PrintElf.ml
	$(OCAMLBUILD) $(OCB_OPTIONS) PrintElf.native

printelfidentification: examples/PrintElfIdentification.ml
	$(OCAMLBUILD) $(OCB_OPTIONS) PrintElfIdentification.native

.PHONY: all printelf clean

clean:
	rm -rf _build PrintElf.native
