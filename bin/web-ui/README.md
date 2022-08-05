# Web Interface

## Setup

```bash
cd $(git rev-parse --show-toplevel)
opam sw create . 4.14.0 --no-install
eval $(opam env)
opam install ocamlformat.0.23.0 js_of_ocaml js_of_ocaml-ppx
cd bin/web-ui
dune build
```

Once this is done, the [`index.html`](index.html) can be loaded and should
properly work.
