# Evaluation Modes

## Lazy Mode

Pointers represent positive-to-negative ports in polarized nets. This causes the
memory format to coincide perfectly with how IC terms are written textually. It
is a direct improvement of [HVM1](https://github.com/HigherOrderCO/hvm1). It is
implemented in this repository.

### Strengths:

- Efficient lazy evaluation
- Lévy Optimality (minimal β-reduction)
- Very fast single-core evaluation
- Compiles to efficient C (often, faster than GHC)

### Drawbacks:

- WHNF may return a pending variable
- Requires global garbage collection
- Parallelism is still an open problem

## Strict Mode

Pointers represent aux-to-main ports, resulting in a tree-like memory format. It
is implemented in a [separate repository](https://github.com/HigherOrderCO/hvm3-strict),
and will be merged later.

### Strengths:

- Efficient parallel evaluation
- Does not require global garbage collection

### Drawbacks:

- Lazy evaluation is impossible

- Not Lévy Optimal (can waste β-reductions)
