ENV ?= prod

.PHONY: setup
setup:
	source .venv/bin/activate && pip install -r requirements.txt

# Build dist/lambda.zip
.PHONY: dist
dist:
	rm -rf dist/*
	mkdir -p dist
	cp main.py dist/
	cp -r .venv/lib/python3.8/site-packages/* dist/
	cd dist && zip -r lambda.zip .

# Copy dist/lambda.zip to S3, for use in CloudFormation templates
.PHONY: s3.cp
s3.cp: dist
	aws --profile restyled s3 cp dist/lambda.zip \
	  s3://ops.restyled.io/autoscale.zip

# Upload dist/lambda.zip directly to a conventionally-named Lambda Function
.PHONY: deploy
deploy: dist
	aws --profile restyled lambda update-function-code \
	  --function-name restyled-$(ENV)-autoscale \
	  --zip-file fileb://dist/lambda.zip
