#!/bin/sh
aws s3 cp --recursive --only-show-errors \
  s3://ops.restyled.io/docker-machine/ /root/.docker/machine

"$@"

if aws s3 sync --delete --only-show-errors \
  /root/.docker/machine/ s3://ops.restyled.io/docker-machine/; then
  echo "Docker Machine state synchronized"
fi
