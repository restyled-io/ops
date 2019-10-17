.PHONY: build
build:
	docker build --tag restyled/ops .

.PHONY: shell
shell: build
	docker run --interactive --tty --rm \
	  --env-file .env \
	  --volume /var/run/docker.sock:/var/run/docker.sock \
	  restyled/ops bash

# Declared as empty to help tab-completion
RELEASE_TAG =? ""

.PHONY: release
release: build
	@[ -n "$(RELEASE_TAG)" ] || { echo "RELEASE_TAG unset" >&2; exit 1; }
	docker tag restyled/ops restyled/ops:$(RELEASE_TAG)
	docker push restyled/ops:$(RELEASE_TAG)
	git tag -s -m "$(RELEASE_TAG)" "$(RELEASE_TAG)"
	git push --follow-tags
