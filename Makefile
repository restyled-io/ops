AWS ?= aws --profile restyled
ENV ?= prod

MACHINES_USER_DATA_VERSION ?=

.PHONY: machines.user-data.release
machines.user-data.release:
	[ -n "$(MACHINES_USER_DATA_VERSION)" ]
	$(AWS) s3 cp --acl public-read \
	  files/machines/user-data \
	  s3://infra.restyled.io/machines/$(ENV)/user-data-v$(MACHINES_USER_DATA_VERSION)
	@printf "=> Update UserDataVersion to %%s in %%s and deploy\n" \
	  $(MACHINES_USER_DATA_VERSION) \
	  cg-app/stacks/restyled.760048635536/us-east-1/prod-machines.yaml
