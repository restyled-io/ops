.PHONY: shell
shell: build
	docker build --tag restyled/ops .
	docker run --interactive --tty --rm \
	  --env-file .env \
	  --volume /var/run/docker.sock:/var/run/docker.sock \
	  restyled/ops bash
