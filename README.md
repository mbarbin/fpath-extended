# fpath-extended

[![CI Status](https://github.com/mbarbin/fpath-extended/workflows/ci/badge.svg)](https://github.com/mbarbin/fpath-extended/actions/workflows/ci.yml)
[![Coverage Status](https://coveralls.io/repos/github/mbarbin/fpath-extended/badge.svg?branch=main)](https://coveralls.io/github/mbarbin/fpath-extended?branch=main)

This library adds a few functions to the
[fpath](https://opam.ocaml.org/packages/fpath/) package to be used by projects
using [base](https://opam.ocaml.org/packages/base/).

## Usage

The intended usage is to refer to the module `Fpath_extended` alongside `Fpath`
for the needed bits, and keep on simply using `Fpath` as usual when no extended
functionality is required.

For example, Fpath_extended adds a sexp serializer and hash functions to
`Fpath.t`, thus allowing for example:

```ocaml file=example.ml
open! Base

let create_fpath_hashtbl () = Hashtbl.create (module Fpath_extended)
```

and in the mli

```ocaml file=example.mli
open! Base

val create_fpath_hashtbl : unit -> _ Hashtbl.M(Fpath).t
```

## Motivations

This experimental package is not meant to be published to the opam repository,
nor is it meant for public consumption.

Really, I've just used this package as an excuse to toy with opam and experiment
with some ways extension packages may be designed and distributed.

The repo itself is public so that I can publish its releases into my personal
[opam-repository](https://github.com/mbarbin/opam-repository), install them in
my local opam switches and refer to these packages from the workflow actions of
other repos.

## Code documentation

The code documentation of the latest release is built with `odoc` and published
to `GitHub` pages [here](https://mbarbin.github.io/fpath-extended).
