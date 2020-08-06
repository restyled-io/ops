AWS ?= aws --profile restyled
ENV ?= prod

.PHONY: machines.update
machines.update: STACK=machines
machines.update: stack.update

.PHONY: services.update
services.update: STACK=services
services.update: stack.update

STACK ?=

.PHONY: stack.create
stack.create:
	$(AWS) cloudformation create-stack \
	  --stack-name "$(ENV)-$(STACK)" \
	  --template-body "$$(cat "infra/stacks/$(STACK).yaml")" \
	  --capabilities CAPABILITY_NAMED_IAM \
	  --parameter "ParameterKey=Environment,ParameterValue=$(ENV)"
	$(AWS) cloudformation wait stack-create-complete \
	  --stack-name "$(ENV)-$(STACK)"

.PHONY: stack.update
stack.update:
	$(AWS) cloudformation update-stack \
	  --stack-name "$(ENV)-$(STACK)" \
	  --template-body "$$(cat "infra/stacks/$(STACK).yaml")" \
	  --capabilities CAPABILITY_NAMED_IAM
	$(AWS) cloudformation wait stack-update-complete \
	  --stack-name "$(ENV)-$(STACK)"

PARAMETER_KEY ?=
PARAMETER_VALUE ?=

.PHONY: stack.update-parameters
stack.update-parameters:
	$(AWS) cloudformation update-stack \
	  --stack-name "$(ENV)-$(STACK)" \
	  --use-previous-template \
	  --parameter "ParameterKey=$(PARAMETER_KEY),ParameterValue=$(PARAMETER_VALUE)" \
	  --capabilities CAPABILITY_NAMED_IAM
	$(AWS) cloudformation wait stack-update-complete \
	  --stack-name "$(ENV)-$(STACK)"

.PHONY: stack.delete
stack.delete:
	$(AWS) cloudformation delete-stack \
	  --stack-name "$(ENV)-$(STACK)"
	$(AWS) cloudformation wait stack-delete-complete \
	  --stack-name "$(ENV)-$(STACK)"

.PHONY: agent.update
agent.update:
	docker build --tag restyled/agent .
	docker push restyled/agent
