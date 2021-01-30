AWS ?= aws --profile restyled
ENV ?= prod

.PHONY: deploy
deploy:
	$(AWS) s3 sync ./cg-app/ s3://cg-app-cfstack-bucket/ --delete

STACK_NAME     ?=
STACK_TEMPLATE ?= $(STACK_NAME).yaml
RAIN           ?= rain --profile restyled

.PHONY: infra.stacks.services.update
infra.stacks.services.update:
	$(AWS) cloudformation update-stack \
	  --stack-name "$(ENV)-services" \
	  --template-body "$$(cat "infra/stacks/services.yaml")" \
	  --parameters \
	    ParameterKey=Environment,UsePreviousValue=true \
	    ParameterKey=RestyledImage,UsePreviousValue=true \
	    ParameterKey=RestylerImage,UsePreviousValue=true \
	    ParameterKey=AppsWebhooksDesiredCount,UsePreviousValue=true \
	  --capabilities CAPABILITY_NAMED_IAM
	$(AWS) cloudformation wait stack-update-complete \
	  --stack-name "$(ENV)-services"

.PHONY: infra.stacks.machines.update
infra.stacks.machines.update:
	$(AWS) cloudformation update-stack \
	  --stack-name "$(ENV)-machines" \
	  --template-body "$$(cat "infra/stacks/machines.yaml")" \
	  --parameters \
	    ParameterKey=Environment,UsePreviousValue=true \
	    ParameterKey=InstanceKeyPair,UsePreviousValue=true \
	    ParameterKey=RestyledHost,UsePreviousValue=true \
	    ParameterKey=DesiredCapacity,UsePreviousValue=true \
	    ParameterKey=UserDataVersion,UsePreviousValue=true \
	    ParameterKey=AgentVersion,UsePreviousValue=true \
	  --capabilities CAPABILITY_NAMED_IAM
	$(AWS) cloudformation wait stack-update-complete \
	  --stack-name "$(ENV)-machines"

USER_DATA_VERSION ?=

.PHONY: infa.files.machines.update
infra.files.machines.update:
	[ -n "$(USER_DATA_VERSION)" ]
	$(AWS) s3 cp --acl public-read \
	  infra/files/machines/user-data \
	  s3://infra.restyled.io/machines/$(ENV)/user-data-v$(USER_DATA_VERSION)
