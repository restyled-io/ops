all: setup setup.lint setup.tools build lint test

.PHONY: setup
setup:
	stack setup $(STACK_ARGUMENTS)
	stack build $(STACK_ARGUMENTS) --dependencies-only --test --no-run-tests

.PHONY: setup.lint
setup.lint:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool \
	  hlint \
	  weeder

.PHONY: setup.tools
setup.tools:
	stack install $(STACK_ARGUMENTS) --copy-compiler-tool \
	  brittany \
	  fast-tags \
	  stylish-haskell

.PHONY: build
build:
	stack build $(STACK_ARGUMENTS) --pedantic --test --no-run-tests

.PHONY: lint
lint:
	stack exec $(STACK_ARGUMENTS) hlint app src test
	stack exec $(STACK_ARGUMENTS) weeder .

.PHONY: test
test:
	stack build $(STACK_ARGUMENTS) --test
