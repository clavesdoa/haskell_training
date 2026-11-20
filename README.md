# Haskell Training

A collection of small Haskell exercises written while learning the language,
organised as a standard [cabal](https://www.haskell.org/cabal/) package. They come
mostly from *Real World Haskell* (the files with `{-- snippet ... --}` markers)
and the University of Edinburgh *Informatics 1 – Functional Programming* course.

## Layout

```
haskell-training.cabal   package description (library, executables, tests)
Makefile                 shortcuts for common cabal commands
src/                     library: one module per exercise topic
app/                     executables (each has a `main`)
test/Spec.hs             test suite
data/                    sample input files
.github/workflows/ci.yml GitHub Actions CI
```

### Library modules (`src/`)

| Module | Topic |
|--------|-------|
| `Edinburgh` | Recursion, list comprehensions, `zip`, folds, a `Nat` data type |
| `MyLength`, `Palindrome`, `SquareSums`, `Sublists` | Basic list recursion |
| `SafeListFuns`, `MySecond` | Total versions of partial functions using `Maybe` |
| `SplitWith` | Splitting a list on a predicate |
| `DigitOps` | Re-implementing `Data.Char` helpers (`isDigit`, `toUpper`, ...) |
| `List`, `Tree` | User-defined recursive data types |
| `ExpressionTrees` | Arithmetic expression trees and propositional logic (evaluation, satisfiability) |
| `Translation` | How type classes translate into explicit dictionary passing |
| `MyIO`, `MyEcho` | A home-made IO monad and an echo program built on it |
| `Parser` | A monadic parser-combinator library (`Monad`, `MonadPlus`, `Alternative`) |

### Executables (`app/`)

| Name | Source | What it does |
|------|--------|--------------|
| `interact-with` | `InteractWith.hs` | Reads a file, upper-cases it, writes another file |
| `wc` | `WC.hs` | Counts words on stdin |
| `my-echo` | `Echo.hs` | Echoes stdin lines in upper case using `MyIO` |

## Requirements

- **GHC** (tested with 9.6.6)
- **cabal-install** 3.x
- **GNU make** (optional, for the shortcuts)

On Debian/Ubuntu: `sudo apt install ghc cabal-install make`.
Alternatively, use [GHCup](https://www.haskell.org/ghcup/) to install GHC, cabal and
the Haskell Language Server (editor support) in your home directory.

Optional: `hlint` for lint suggestions (`sudo apt install hlint`).

## Usage

```sh
make            # build everything and run the tests
make build      # cabal build all
make test       # cabal test
make run-demo   # run each executable on sample input
make repl       # GHCi with all library modules loaded
make lint       # hlint (if installed)
make clean      # cabal clean
```

Or use cabal directly:

```sh
cabal build
cabal test
cabal run wc < data/quux.txt
cabal run interact-with -- data/hello-in.txt out.txt
cabal repl                        # then e.g.  :m + ExpressionTrees
```

Example GHCi session:

```
ghci> import ExpressionTrees
ghci> satisfiable p1
True
ghci> showExp e0
"(2+(3*3))"
```

For a quick look at a single file without cabal: `ghci -isrc src/Edinburgh.hs`.

## Adding an exercise

1. Create `src/NewTopic.hs` starting with `module NewTopic where`.
2. Add `NewTopic` to `exposed-modules` in `haskell-training.cabal`.
3. Optionally add checks for it to `test/Spec.hs`.

## Continuous integration

`.github/workflows/ci.yml` builds the package, runs the tests and runs the demo
programs on every push and pull request, using GHC 9.6.6 on GitHub Actions.
