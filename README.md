# http/un

http/un is a high-performance, memory-efficient, scalable and web library for
OCaml. It uses the [Angstrom][angstrom] and [Faraday][faraday]
libraries for parsing and serialization.

[angstrom]: https://github.com/inhabitedtype/angstrom
[faraday]: https://github.com/inhabitedtype/faraday

httpun is a fork of [httpaf](https://github.com/inhabitedtype/httpaf) that
fixes bugs in the original work and adds additional features. See
[FORK.md](./FORK.md) for more details on those changes.

## Installation

```bash
opam install httpun
```

## Usage

Check the [`examples`][examples] folder.

To run the black-box HTTP/1.1 `h1spec` suite from the Nix dev shell:

```bash
nix develop -c make h1spec
```

[examples]: https://github.com/anmonteiro/httpun/tree/master/examples

## License

BSD3, see [LICENSE](./LICENSE) files for its text.
