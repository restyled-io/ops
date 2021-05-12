.PHONY: deploy
deploy:
	AWS_PROFILE=restyled ./aws-sync/sync cg-app cg-app-cfstack-bucket

AWS ?= aws --profile restyled
ENV ?= prod

STACK_NAME     ?=
STACK_TEMPLATE ?= $(STACK_NAME).yaml
RAIN           ?= rain --profile restyled

.PHONY: infra.stacks.services.deploy
infra.stacks.services.deploy:
	$(AWS) s3 cp --acl public-read \
	  infra/stacks/services.yaml \
	  s3://infra.restyled.io/templates/$(ENV)/services.yaml
	$(AWS) cloudformation deploy \
	  --stack-name "$(ENV)-services" \
	  --template-file "infra/stacks/services.yaml" \
	  --capabilities CAPABILITY_NAMED_IAM

.PHONY: infra.stacks.machines.deploy
infra.stacks.machines.deploy:
	$(AWS) s3 cp --acl public-read \
	  infra/stacks/machines.yaml \
	  s3://infra.restyled.io/templates/$(ENV)/machines.yaml
	$(AWS) cloudformation deploy \
	  --stack-name "$(ENV)-machines" \
	  --template-file "infra/stacks/machines.yaml" \
	  --capabilities CAPABILITY_NAMED_IAM

USER_DATA_VERSION ?=

.PHONY: infa.files.machines.deploy
infra.files.machines.deploy:
	[ -n "$(USER_DATA_VERSION)" ]
	$(AWS) s3 cp --acl public-read \
	  infra/files/machines/user-data \
	  s3://infra.restyled.io/machines/$(ENV)/user-data-v$(USER_DATA_VERSION)
