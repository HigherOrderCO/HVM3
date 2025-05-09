#!/bin/bash
# Simple test script for the IC-Prove prover

# Ensure we have the required HVM3 build
if ! command -v cabal &> /dev/null; then
    echo "Cabal not found. Please install Haskell and Cabal first."
    echo "On macOS: brew install ghc cabal-install"
    echo "Or visit: https://www.haskell.org/ghcup/"
    exit 1
fi

# Build the project
echo "Building HVM3 with Prover..."
cabal build

echo "Testing logic_syntax.hvm..."
cabal run hvm3 -- run examples/logic_syntax.hvm

echo "Testing prover_rules.hvm..."
cabal run hvm3 -- run examples/prover_rules.hvm

echo "Testing axioms_mp_sprint0.hvm..."
cabal run hvm3 -- run examples/axioms_mp_sprint0.hvm

echo "Testing target_mp_sprint0.hvm..."
cabal run hvm3 -- run examples/target_mp_sprint0.hvm

# Run the prover on our test case
echo "Running prover on the P→Q example..."
cabal run hvm3 -- prove \
  --target examples/target_mp_sprint0.hvm \
  --axioms examples/axioms_mp_sprint0.hvm \
  --rules examples/prover_rules.hvm \
  --budget_ct 1000

echo "Test complete." 