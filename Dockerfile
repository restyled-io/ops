FROM node:10.12.0-alpine
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

ENV LANG en_US.UTF-8
RUN apk add --update bash curl

# Add Heroku CLI
ENV HEROKU_VERSION=7.16.8
RUN npm install -g heroku@$HEROKU_VERSION

# Add Docker client
ENV DOCKER_ARCHIVE docker-17.03.1-ce.tgz
ENV DOCKER_SRC_URL https://get.docker.com/builds/Linux/x86_64/$DOCKER_ARCHIVE
RUN \
  curl -fsSLO "$DOCKER_SRC_URL" && \
  tar --strip-components=1 -xvzf "$DOCKER_ARCHIVE" -C /usr/local/bin

COPY files /
