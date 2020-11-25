AWS ?= aws --profile restyled
ENV ?= prod

STACK_NAME     ?=
STACK_TEMPLATE ?= $(STACK_NAME).yaml
RAIN           ?= rain --profile restyled

.PHONY: deploy
deploy:
	[ -n "$(STACK_NAME)" ]
	$(RAIN) deploy "infra/stacks/$(STACK_TEMPLATE)" "$(ENV)-$(STACK_NAME)"

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

.PHONY: infra.stacks.ops.update
infra.stacks.ops.update:
	$(AWS) cloudformation update-stack \
	  --stack-name "$(ENV)-ops" \
	  --template-body "$$(cat "infra/stacks/ops.yaml")" \
	  --parameters \
	    ParameterKey=Environment,UsePreviousValue=true \
	  --capabilities CAPABILITY_NAMED_IAM
	$(AWS) cloudformation wait stack-update-complete \
	  --stack-name "$(ENV)-ops"

.PHONY: infra.stacks.blog.delete
infra.stacks.blog.delete:
	$(AWS) cloudformation delete-stack \
	  --stack-name "$(ENV)-blog"
	$(AWS) cloudformation wait stack-delete-complete \
	  --stack-name "$(ENV)-blog"

.PHONY: infra.stacks.blog.create
infra.stacks.blog.create:
	$(AWS) cloudformation create-stack \
	  --stack-name "$(ENV)-blog" \
	  --template-body "$$(cat "infra/stacks/static-website.yaml")" \
	  --parameters \
	    ParameterKey=Environment,ParameterValue=$(ENV) \
	    ParameterKey=Alias,ParameterValue=blog.restyled.io \
	  --capabilities CAPABILITY_NAMED_IAM
	$(AWS) cloudformation wait stack-create-complete \
	  --stack-name "$(ENV)-blog"

.PHONY: infra.stacks.blog.update
infra.stacks.blog.update:
	$(AWS) cloudformation update-stack \
	  --stack-name "$(ENV)-blog" \
	  --template-body "$$(cat "infra/stacks/static-website.yaml")" \
	  --parameters \
	    ParameterKey=Environment,UsePreviousValue=true \
	    ParameterKey=Alias,UsePreviousValue=true \
	  --capabilities CAPABILITY_NAMED_IAM
	$(AWS) cloudformation wait stack-update-complete \
	  --stack-name "$(ENV)-blog"

.PHONY: infra.stacks.docs.delete
infra.stacks.docs.delete:
	$(AWS) cloudformation delete-stack \
	  --stack-name "$(ENV)-docs"
	$(AWS) cloudformation wait stack-delete-complete \
	  --stack-name "$(ENV)-docs"

.PHONY: infra.stacks.docs.create
infra.stacks.docs.create:
	$(AWS) cloudformation create-stack \
	  --stack-name "$(ENV)-docs" \
	  --template-body "$$(cat "infra/stacks/static-website.yaml")" \
	  --parameters \
	    ParameterKey=Environment,ParameterValue=$(ENV) \
	    ParameterKey=Alias,ParameterValue=docs.restyled.io \
	  --capabilities CAPABILITY_NAMED_IAM
	$(AWS) cloudformation wait stack-create-complete \
	  --stack-name "$(ENV)-docs"

.PHONY: infra.stacks.docs.update
infra.stacks.docs.update:
	$(AWS) cloudformation update-stack \
	  --stack-name "$(ENV)-docs" \
	  --template-body "$$(cat "infra/stacks/static-website.yaml")" \
	  --parameters \
	    ParameterKey=Environment,UsePreviousValue=true \
	    ParameterKey=Alias,UsePreviousValue=true \
	  --capabilities CAPABILITY_NAMED_IAM
	$(AWS) cloudformation wait stack-update-complete \
	  --stack-name "$(ENV)-docs"

USER_DATA_VERSION ?=

.PHONY: infa.files.machines.update
infra.files.machines.update:
	[ -n "$(USER_DATA_VERSION)" ]
	$(AWS) s3 cp --acl public-read \
	  infra/files/machines/user-data \
	  s3://infra.restyled.io/machines/$(ENV)/user-data-v$(USER_DATA_VERSION)
