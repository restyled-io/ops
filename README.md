# Restyled Ops

Support image and tooling for the development, deployment, and operation of
Restyled.

## Environment

Environment variables are required to do anything meaningful:

```sh
# shellcheck disable=SC2034

# To run anything at all
AWS_ACCESS_KEY_ID=...
AWS_SECRET_ACCESS_KEY=...

# To interact with Heroku-backed Reds (scale-watch, fix-redis-url)
HEROKU_EMAIL=...
HEROKU_API_KEY=...

# To perform scaling operations that add/remove Restyle Machines via API
RESTYLED_API_HOST=...
RESTYLED_API_TOKEN=...

# For notify (used by scale-watch)
PUSHOVER_API_KEY=...
PUSHOVER_USER_KEY=...
```

## Local

```console
docker build --tag {tag} .
docker run --rm --env-file "$PWD"/.env {tag} {command}
```

## `bin/self`

This script manages running a persistent ops container to handle automatic
scaling based on our main queue depth. The script creates the EC2 instance if
necessary and supports the following sub-commands:

```console
bin/self start
```

Starts the scale-watch container.

```console
bin/self start
```

Stops the scale-watch container.

```console
bin/self tail
```

Tails the scale-watch container.

```console
bin/self exec {command}
```

Runs a command in the scale-watch container.

## FAQ

**Why `exec` and not `run`?** Well, because we currently push/pull our Docker
Machine state from S3 during startup/shutdown (see `entrypoint.sh`). We do this
naively such that concurrent access doesn't work. So, as much as possible, we
try to push for executing commands in the (only) running container, instead of
launching new ones that would operate on stale state.

**Why does `exec docker-machine ssh` not work?** Sometimes it does, but because
we push/pull Docker Machine state naively, we don't correctly preserve
permissions of the SSH keys. If you are attempting to SSH into an instance that
was created by a different ops container, it won't work.

---

[LICENSE](./LICENSE)
