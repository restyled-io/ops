#!/usr/bin/env bash
set -eu

aws s3 cp --recursive --only-show-errors \
  s3://ops.restyled.io/docker-machine/ /root/.docker/machine
find /root/.docker/machine -name id_rsa -exec chmod 600 {} \;

"$@" &
pid=$!

trap 'kill -INT "$pid" 2>/dev/null' INT
trap 'kill -TERM "$pid" 2>/dev/null' QUIT
trap 'kill -QUIT "$pid" 2>/dev/null' TERM

wait "$pid" || true

echo "Pushing Docker Machine state"
if aws s3 sync --delete --only-show-errors \
  /root/.docker/machine/ s3://ops.restyled.io/docker-machine/; then
  echo "Docker Machine state pushed"
fi
