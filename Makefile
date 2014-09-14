all:
	ocamlbuild -use-ocamlfind ovideo.cma ovideo.cmxa

test: all
	ocamlbuild -use-ocamlfind test_bits.byte test_dct.byte

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f
