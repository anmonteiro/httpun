.PHONY: all build clean test examples

build:
	dune build @install

all: build

test:
	dune runtest --no-buffer

examples:
	dune build @examples

watch:
	dune build {httpun,httpun-async,httpun-lwt-unix}.install @runtest --watch

install:
	dune install

uninstall:
	dune uninstall

clean:
	rm -rf _build *.install
