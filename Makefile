all:
	ocamlbuild -use-ocamlfind -cflag -unsafe ovideo.cma ovideo.cmxa

test: all
	ocamlbuild -use-ocamlfind test_bits.byte test_dct.byte test_motion.byte

clean:
	ocamlbuild -clean
	-find . -name "*~" | xargs rm -f
