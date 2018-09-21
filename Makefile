all: setup build lint test

release: clean build lint test image.build image.release

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install --copy-compiler-tool hlint weeder

.PHONY: clean
clean:
	stack clean

.PHONY: build
build:
	stack build --fast --pedantic --test --no-run-tests

.PHONY: lint
lint:
	stack exec hlint .
	stack exec weeder .

.PHONY: test
test:
	stack test

.PHONY: update
update:
	stack exec restyled-ops -- --no-notify update --template new

.PHONY: install
install:
	stack install

.PHONY: image.build
image.build:
	docker build --tag restyled/ops .

.PHONY: image.release
image.release:
	docker push restyled/ops
