.PHONY: repl tests test fmt lint celan

all:
	dune build

repl:
	dune build ./slarn.exe && rlwrap _build/default/slarn.exe

test:
	dune runtest

clean:
	@$(RM) -r _build

fmt:
	dune build @fmt --auto-promote

lint:
	dune build @lint --force

release:
	dune build --profile=release
	dune runtest --profile=release
