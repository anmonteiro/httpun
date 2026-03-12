.PHONY: all build clean test examples h1spec

build:
	dune build @install

all: build

test:
	dune runtest --no-buffer

examples:
	dune build @examples

h1spec:
	./scripts/run-h1spec.sh

watch:
	dune build {httpun,httpun-async,httpun-lwt-unix}.install @runtest --watch

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build *.install
