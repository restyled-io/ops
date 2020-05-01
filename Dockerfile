FROM node:10.12.0-alpine
LABEL maintainer="Pat Brisbin <pbrisbin@gmail.com>"

ENV LANG en_US.UTF-8
RUN apk add --update --no-cache \
  autoconf=2.69-r2 \
  automake=1.16.1-r0 \
  bash=4.4.19-r1 \
  curl=7.61.1-r3 \
  g++=6.4.0-r9 \
  gcc=6.4.0-r9 \
  git=2.18.4-r0 \
  make=4.2.1-r2 \
  openssh-client=7.7_p1-r4 \
  py-pip=10.0.1-r0

# Compilations
ENV JO_VERSION 1.3
# hadolint ignore=DL3003
RUN \
  cd /tmp && \
  curl -L -O https://github.com/jpmens/jo/releases/download/$JO_VERSION/jo-$JO_VERSION.tar.gz && \
  tar xzf "jo-$JO_VERSION.tar.gz" && \
  ( cd "jo-$JO_VERSION" && \
    autoreconf -i && \
    ./configure && \
    make check && \
    make install \
  ) && \
  rm -rf "/tmp/jo-$JO_VERSION"

# Package installs
RUN pip install --upgrade pip==20.1
ENV ECS_DEPLOY_VERSION 1.10.1
RUN pip install ecs-deploy==$ECS_DEPLOY_VERSION
ENV HEROKU_VERSION 7.35.0
RUN npm install -g heroku@$HEROKU_VERSION

# Binaries
ENV JQ_VERSION 1.6
RUN \
  curl -L -o /usr/local/bin/jq https://github.com/stedolan/jq/releases/download/jq-$JQ_VERSION/jq-linux64 && \
  chmod +x /usr/local/bin/jq
ENV DOCKER_VERSION 19.03.3
RUN \
  curl -fsSLO https://download.docker.com/linux/static/stable/x86_64/docker-$DOCKER_VERSION.tgz && \
  tar --strip-components=1 -xvzf "docker-$DOCKER_VERSION.tgz" -C /usr/local/bin
ENV DOCKER_MACHINE_VERSION v0.16.0
RUN \
  curl -L -o /usr/local/bin/docker-machine https://github.com/docker/machine/releases/download/$DOCKER_MACHINE_VERSION/docker-machine-Linux-x86_64 && \
  chmod +x /usr/local/bin/docker-machine

COPY files /
