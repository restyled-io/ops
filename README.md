# Restyled Ops

Tooling for the deployment and operation of Restyled.

## `cg-app/`

Fully descriptive CloudFormation templates, deployed using [CloudGenesis][].

[CloudGenesis]: https://github.com/LifeWay/CloudGenesis

## `files/`

Static files that need manual deployment to S3. See `Makefile`.

## `lambda/check-redis-url/`

**Purpose**: Checks if our Heroku Add-on has changed its `REDIS_URL`, and
updates our SSM Parameter accordingly.

**Deployment story**: `yarn run dist && yarn run s3.cp` builds and copies a zip
file to S3, which is expected by the `machines` CloudFormation Stack. `yarn run
release.prod` can be used to update the Function directly.

## `lambda/record-metrics/`

**Purpose**: Records Queue Depth to CloudWatch every minute.

**Deployment story**: `make s3.cp` builds and copies a zip file to S3, which is
expected by the `machines` CloudFormation Stack. `make release.prod` can be used
to update the Function directly.

## `lambda/logdna-cloudwatch/`

**Purpose**: Forwards CloudWatch logs to LogDNA. Simplified and improved version
of their official one.

**Deployment story**: `yarn run dist && yarn run s3.cp` builds and copies a zip
file to S3, which is expected by the `services` CloudFormation Stack. `yarn run
release.prod` can be used to update the Function directly.

---

[LICENSE](./LICENSE)
