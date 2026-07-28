.PHONY: all build clean test

all: build

build:
	dune build
	rm -f nomad
	cp _build/default/bin/nomad.exe nomad
	chmod +x nomad

clean:
	dune clean
	rm -f nomad

test:
	dune runtest
