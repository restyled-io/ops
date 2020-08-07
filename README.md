# Restyled Ops

Tooling for the deployment and operation of Restyled.

## `lambda/check-redis-url/`

**Purpose**: Checks if our Heroku Add-on has changed its `REDIS_URL`, and
updates our SSM Parameter accordingly.

**Deployment story**: `yarn run dist && yarn run s3.cp` builds and copies a zip
file to S3, which is expected by the `machines` CloudFormation Stack. `yarn run
deploy` can be used to update the Function directly.

## `lambda/record-metrics/`

**Purpose**: Records Queue Depth to CloudWatch every minute.

**Deployment story**: `make s3.cp` builds and copies a zip file to S3, which is
expected by the `machines` CloudFormation Stack. `make deploy` can be used to
update the Function directly.

## `lambda/logdna-cloudwatch/`

**Purpose**: Forwards CloudWatch logs to LogDNA. Simplified and improved version
of their official one.

**Deployment story**: `yarn run dist && yarn run s3.cp` builds and copies a zip
file to S3, which is expected by the `services` CloudFormation Stack. `yarn run
deploy` can be used to update the Function directly.

## `infra/`

**Purpose**: CloudFormation Stack definitions for Restyled infrastructure. All
Stacks accept an `Environment` parameter for scoping resources. SSM Parameters
under `/restyled/${Environment}/` are the only external infrastructure that is
expected to exist.

- `vpc`: core networking; a VPC and Subnets across 3 AZs
- `services`: ECS Cluster and Services
- `machines`: Autoscaling Group of Docker-enabled "Restyle Machines", which
  self-register to accept Restyler Jobs

**Deployment story**: `make infra.stacks.*`

## TODO

- Define EC2/ECS AutoScale policies in CF
- Centralize generic IAM Policy resources in their own Stack?

---

[LICENSE](./LICENSE)
