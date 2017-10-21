all: setup build lint test

release: clean build lint test image.build image.release

.PHONY: setup
setup:
	stack setup
	stack build --dependencies-only --test --no-run-tests
	stack install hlint weeder

.PHONY: clean
clean:
	stack clean

.PHONY: build
build:
	stack build --pedantic --test --no-run-tests

.PHONY: lint
lint:
	hlint .
	weeder .

.PHONY: test
test:
	stack test

.PHONY: install
install:
	stack install

.PHONY: image.build
image.build:
	docker build --tag restyled/ops .

.PHONY: image.release
image.release:
	docker push restyled/ops
