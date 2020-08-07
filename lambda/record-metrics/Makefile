dist/lambda.zip: main.py
	rm -rf dist/*
	mkdir -p dist
	cp main.py dist/
	cp -r .venv/lib/python3.8/site-packages/* dist/
	cd dist && zip -r lambda.zip .

.PHONY: release.prod
release.prod: dist/lambda.zip
	aws --profile restyled s3 cp dist/lambda.zip \
	  s3://ops.restyled.io/record-metrics.zip
	aws --profile restyled lambda update-function-code \
	  --function-name restyled-prod-record-metrics \
	  --zip-file fileb://dist/lambda.zip
