.PHONY: test check

build:
	dune build

utop:
	OCAMLRUNPARAM=b dune utop src

test:
	OCAMLRUNPARAM=b dune exec test/main.exe

play:
	OCAMLRUNPARAM=b dune exec bin/main.exe

zip:
	rm -f vortex.zip
	zip -r vortex.zip . -x@exclude.lst

clean:
	dune clean
	rm -f vortex.zip

doc:
	dune build @doc

docs:
	dune build @doc

cloc:
	ocamlbuild -clean && cloc --by-file --include-lang=OCaml .
