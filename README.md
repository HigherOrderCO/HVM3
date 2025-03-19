# HVM3 - Work In Progress

The **HVM** is an efficient implementation of the [Interaction Calculus](https://github.com/HigherOrderCO/Interaction-Calculus) (IC).

The Interaction Calculus is a new foundation for computing, similar to the
Lambda Calculus, but theoretically more efficient. The HVM is an efficient
implementation of this new paradigm, and can be seen as a fast engine for
symbolic computations.

In some ways, it is very similar to Haskell, but it has some key differences:
- Lambdas are fully linear, making it resource-aware
- Lambdas are globally scoped, enabling long-range communication
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
cabal run hvml -- run file.hvml     # runs lazy-mode, interpreted
cabal run hvml -- run file.hvml -c  # runs lazy-mode, compiled

cabal run hvml -- run file.hvms     # runs strict-mode, interpreted (TODO)
cabal run hvml -- run file.hvms -c  # runs strict-mode, compiled (TODO)
```

Note: the `-c` flag will also generate a standalone `.main.c` file, which if you
want, you can compile and run it independently. See examples on the [book/](book/) directory.

## Evaluation Modes

### Lazy Mode

Pointers represent positive-to-negative ports in polarized nets. This causes the
memory format to coincide perfectly with how IC terms are written textually. It
is a direct improvement of [HVM1](https://github.com/HigherOrderCO/hvm1). It is
implemented in this repository.

#### Strengths:

- Efficient lazy evaluation
- Lévy Optimality (minimal β-reduction)
- Very fast single-core evaluation
- Compiles to efficient C (often, faster than GHC)

#### Drawbacks:

- WHNF may return a pending variable
- Requires global garbage collection
- Parallelism is still an open problem

### Strict Mode

Pointers represent aux-to-main ports, resulting in a tree-like memory format. It
is implemented in a [separate repository](https://github.com/HigherOrderCO/hvm3-strict),
and will be merged later.

#### Strengths:

- Efficient parallel evaluation
- Does not require global garbage collection

#### Drawbacks:

- Lazy evaluation is impossible

- Not Lévy Optimal (can waste β-reductions)

## Performance

Benchmarks will be added later. In the few programs tested, HVM3 is up to 42x
faster single-core than Bend, due to its compiler (Bend was interpreted). It is
also 2x-3x faster than Node.js and Haskell in the first program I tested, but
possibly slower in others. HVM3 is a work-in-progress. It is currently single
threaded. Threading (both on CPU and GPU) will be added later.
