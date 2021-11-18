
ifeq (, $(shell which ocamlformat))
	FORMAT =
else
	FORMAT = @fmt --auto-promote
endif

build:
	dune build src/main.exe $(FORMAT)

run: build
	./_build/default/src/main.exe

clean:
	dune clean
	rm -rf _build
