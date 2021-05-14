.PHONY: deploy
deploy:
	AWS_PROFILE=restyled ./aws-sync/sync cg-app cg-app-cfstack-bucket

AWS ?= aws --profile restyled
ENV ?= prod

USER_DATA_VERSION ?=

.PHONY: machines.user-data.release
machines.user-data.release:
	[ -n "$(USER_DATA_VERSION)" ]
	$(AWS) s3 cp --acl public-read \
	  files/machines/user-data \
	  s3://infra.restyled.io/machines/$(ENV)/user-data-v$(USER_DATA_VERSION)
