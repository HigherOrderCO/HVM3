# HVM3 - Work In Progress

The **HVM** is an efficient implementation of the [Interaction Calculus](https://github.com/VictorTaelin/Interaction-Calculus) (IC).

The Interaction Calculus is a new foundation for computing, similar to the
Lambda Calculus, but theoretically optimal. The HVM is an efficient
implementation of this new paradigm, and can be seen as a fast engine for
symbolic computations.

In some ways, it is very similar to Haskell, but it has some key differences:
- Lambdas must be linear or affine, making it resource-aware
- Lambdas have no scope boundaries, enabling global substitutions
- First-class duplications allow a term to be copied into to two locations
- First-class superpositions allow 2 terms to be stored in 1 location

These primitives allow HVM to natively represent concepts that are not present
in the traditional λ-Calculus, including continuations, linear HOAS interpreters
and mutable references. Moreover, superpositions and duplications allow it to
perform optimal beta-reduction, allowing some expressions to be evaluated with
an exponential speedup. Finally, being fully affine makes its garbage collector
very efficient, and greatly simplifies parallelism.

The HVM3 is the successor to HVM1 and HVM2, combining their strengths. It aims
to be the main compile target of [Bend](https://github.com/HigherOrderCO/Bend).
It is a WIP under active development.

## Specifications

- [IC.md](./IC.md): Interaction Calculus, the theoretical foundation behind HVM

- [HVM.md](./HVM.md): the HVM language, which extends IC with pragmatic primitives

## Install

1. Install Cabal.

3. Clone this repository.

3. Run `cabal install`.

## Usage

```bash
hvm run file.hvml    # runs interpreted (slow)
hvm run file.hvml -c # runs compiled (fast)
```

Note: the `-c` flag will also generate a standalone `.main.c` file, which if you
want, you can compile and run it independently. See examples on the [book/](book/) directory.
