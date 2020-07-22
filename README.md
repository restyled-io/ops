# Restyled Ops

Tooling for the deployment and operation of Restyled.

## Agent

**Purpose**: Launched on each Restyle Machine via user-data to respond to
Autoscaling Lifecycle Hooks. Registers to accept Restyler Jobs when launching.
De-registers and waits for in-flight Jobs before terminating.

**Source files**: top-level Haskell package.

**Deployment story**: `make agent.update` builds and pushes `restyled/agent`.

## Autoscale

**Purpose**: Check current queue depth and number of Restyle Machines in
service. If needed, scales things up or down.

**Source files**: `autoscale/`

**Deployment story**: none, for now.

## init-certificates

**Purpose**: wrap non-interactive `openssl` commands to produce TLS certificates
for securing a Docker server. Run on new Restyle Machine instances through
user-data.

**Source files**: `init-certificates/`

**Deployment story**: `make build test release` in source directory.

## check-redis-url

**Purpose**: Checks if our Heroku Add-on has changed its `REDIS_URL`, and
updates our SSM Parameter accordingly.

**Source files**: `check-redis-url/`

**Deployment story**: none, for now.

## logdna-cloudwatch

**Purpose**: Forwards CloudWatch logs to LogDNA. Simplified and improved version
of their official one.

**Source files**: `logdna-cloudwatch/`

**Deployment story**: `yarn run dist && yarn run s3.cp` builds and copies a zip
file to S3, which is expected by the `services` CloudFormation Stack.

## Infra Stacks

**Purpose**: CloudFormation Stack definitions for Restyled infrastructure. All
Stacks accept an `Environment` parameter for scoping resources. SSM Parameters
under `/restyled/${Environment}/` are the only external infrastructure that is
expected to exist.

- `vpc`: core networking; a VPC and Subnets across 3 AZs
- `services`: ECS Cluster and Services

  1. `Webhooks`: Service for processing Webhook messages and launching Restyler
     jobs
  1. `Health`: Scheduled Task to collect and log information
  1. `Reconcile`: Scheduled Task to reconcile Job metadata after a deployment
     may have sent them out of sync
  1. `SyncMarketplace`: Scheduled Task to sync GitHub Marketplace subscriptions

  This Stack also includes `logdna-cloudwatch`.

- `machines`: Autoscaling Group of Docker-enabled "Restyle Machines", which
  self-register to accept Restyler Jobs (by running an Agent).

**Source files**: `infra/stacks/`

**Deployment story**: `make {stack}.update`, `make stack.*`, etc

## TODO

- Bring back prune script on Restyle Machines
- Make autoscale a Lambda in ops stack
- Centralize generic IAM Policy resources in their own Stack?
- Should `agent/` and `init-certificates/` be their own repositories?
  - And get CI/CD

---

[LICENSE](./LICENSE)
