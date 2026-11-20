# Thin shortcuts over cabal, which does the real build work.

CABAL ?= cabal

.PHONY: all build test run-demo repl lint clean help

all: build test

help:
	@echo "make build      - build the library and executables"
	@echo "make test       - run the test suite"
	@echo "make run-demo   - run each executable on sample input"
	@echo "make repl       - open GHCi with all library modules loaded"
	@echo "make lint       - run hlint (if installed)"
	@echo "make clean      - remove build artefacts"

build:
	$(CABAL) build all

test:
	$(CABAL) test --test-show-details=direct

run-demo: build
	$(CABAL) run -v0 interact-with -- data/hello-in.txt data/hello-out.txt && cat data/hello-out.txt
	$(CABAL) run -v0 wc < data/quux.txt
	printf 'hello\nworld\n\n' | $(CABAL) run -v0 my-echo

repl:
	$(CABAL) repl haskell-training

lint:
	@command -v hlint >/dev/null || { echo "hlint not installed (sudo apt install hlint)"; exit 1; }
	hlint src app test

clean:
	$(CABAL) clean
