.PHONY: build
build:
	docker build --tag restyled/ops .

# Declared as empty to help tab-completion
RELEASE_TAG =? ""

.PHONY: release
release: build
	@[ -n "$(RELEASE_TAG)" ] || { echo "RELEASE_TAG unset" >&2; exit 1; }
	docker tag restyled/ops restyled/ops:$(RELEASE_TAG)
	docker push restyled/ops:$(RELEASE_TAG)
	git tag -s -m "$(RELEASE_TAG)" "$(RELEASE_TAG)"
	git push --follow-tags
