.PHONY: build
build:
	docker build --tag restyled/init-certificates .

.PHONY: test
test:
	rm -rf tmp/*
	docker run --rm \
	  --volume "$(PWD)/tmp":/out \
	  restyled/init-certificates -H blah.example.com -o /out

.PHONY: release
release:
	docker push restyled/init-certificates
