# Restyled - Ops

Support for the deployment and operation of Restyled infrastructure.

## Usage

### On CI

```yaml
deploy:
  docker:
    - image: restyled/ops
  steps:
    - setup_remote_docker:
        reusable: true
        version: 17.07.0-ce
    - deploy:
        name: Deploy
        command: |
          heroku container:login
          heroku container:push web
          heroku container:release web
```

### Locally

```console
% export HEROKU_EMAIL=...
% export HEROKU_API_KEY=...
% make shell
bash-4.4# heroku config:set FOO=bar --app restyled-io
```

---

[CHANGELOG](./CHANGELOG.md) | [LICENSE](./LICENSE)
