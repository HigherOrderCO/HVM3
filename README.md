HVM3 - Work In Progress
=======================

**HVM3** will combine the strengths of HVM1 and HVM2 while addressing their
limitations. It aims to be the long-term runtime for
[Bend](https://github.com/HigherOrderCO/Bend). It has 2 modes:

- **HVML**: lazy mode. Pointers represent positive-to-negative ports in
  polarized nets, which coincides with the [Interaction
  Calculus](https://GitHub.com/VictorTaelin/Interaction-Calculus). Strengths:
  efficient lazy evaluation, β-optimality. Drawbacks: 1. `whnf()` may return a
  pending variable; 2.garbage collection is needed; 3. parallelism is less
  pervasive. It is based on [HVM1](https://github.com/HigherOrderCO/hvm1).

- **HVMS**: strict mode. Pointers represent aux-to-main ports, resulting in a
  tree-like memory format. Strengths: efficient massively parallel evaluation and
  no garbage-collection. Drawbacks: not laziness and no optimal evaluation. It
  is based on [HVM2](https://github.com/HigherOrderCO/hvm).

HVM3 is a work-in-progress. Its features are being actively implemented.

Install
-------

1. Install Cabal.

3. Clone this repository.

3. Run `cabal install`.

Usage
-----

```bash
cabal run hvml -- run file.hvml     # runs lazy-mode, interpreted
cabal run hvml -- run file.hvml -c  # runs lazy-mode, compiled

cabal run hvml -- run file.hvms     # runs strict-mode, interpreted (TODO)
cabal run hvml -- run file.hvms -c  # runs strict-mode, compiled (TODO)
```

Note: the `-c` flag will also generate a standalone `.main.c` file, which if you
want, you can compile and run it independently. See examples on the [book/](book/) directory.

Performance
-----------

Benchmarks will be added later. In the few programs tested, HVM3 is up to 42x
faster single-core than Bend, due to its compiler (Bend was interpreted). It is
also 2x-3x faster than Node.js and Haskell in the first program I tested, but
possibly slower in others. HVM3 is a work-in-progress. It is currently single
threaded. Threading (both on CPU and GPU) will be added later.
