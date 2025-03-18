# Contributing to HVM3

Thank you for considering contributing to HVM3!

## How to Contribute

### Reporting Bugs

1. **Check for existing issues:** Before creating a new issue, please search [our issues](https://github.com/HigherOrderCO/HVM3/issues) to see if the bug has already been reported.
2. **Create a new issue:** If you find no similar issue, create a new issue and provide detailed information, including steps to reproduce the problem.

### Suggesting Enhancements

1. **Check for existing suggestions:** Before suggesting a new feature, please check if it's already been suggested in [our issues](https://github.com/HigherOrderCO/HVM3/issues).
2. **Create a new suggestion:** If you find no similar suggestion, create a new issue and provide detailed information about the enhancement and why it would be useful.

### Submitting Changes

1. **Fork the repository:** Create your own fork of the repository on GitHub.
2. **Create a new branch:** Make your changes in a new branch in your forked repository.
3. **Run tests:** Ensure all github workflow tests pass before submitting your changes:
4. **Submit a pull request:** Once your changes are ready, submit a pull request from your branch to the `main` branch of the HVM3 repository.


## Development Setup

1. **Install prerequisites:**
   - Install [Cabal](https://www.haskell.org/cabal/) & [GHC](https://www.haskell.org/ghc/)

2. **Clone your fork:**
   ```
   git clone https://github.com/YOUR-USERNAME/HVM3.git
   cd HVM3
   ```

3. **Install dependencies:**
   ```
   cabal update
   cabal build
   ```

4. **Test your setup:**
   ```
   cabal run hvml -- run book/example.hvml
   ```

## Documentation

Please update documentation when making changes:
- Update comments in code
- Update README.md if relevant
- Add examples for new features in the book/ directory

## Community

- Join discussions on [GitHub Issues](https://github.com/HigherOrderCO/HVM3/issues)
- Reach out to maintainers in the official Discord server & Email: contact@higherorderco.com

We appreciate every contribution!
