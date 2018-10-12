IMAGE_TAG ?= latest
IMAGE_NAME ?= restyled/ops:$(IMAGE_TAG)

.PHONY: build
build:
	docker build --tag "$(IMAGE_NAME)" .

.PHONY: release
release: build
	docker push "$(IMAGE_NAME)"

.PHONY: shell
shell: build
	docker run --interactive --tty --rm \
	  --env HEROKU_EMAIL \
	  --env HEROKU_API_KEY \
	  --volume /var/run/docker.sock:/var/run/docker.sock \
	  "$(IMAGE_NAME)" bash
