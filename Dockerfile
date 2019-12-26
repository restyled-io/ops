FROM node:10.12.0-alpine
MAINTAINER Pat Brisbin <pbrisbin@gmail.com>

ENV LANG en_US.UTF-8
RUN apk add --update bash curl git py-pip

# Add ecs-deploy
RUN pip install --upgrade pip
RUN pip install ecs-deploy

# Add Heroku CLI
ENV HEROKU_VERSION=7.35.0
RUN npm install -g heroku@$HEROKU_VERSION

# Add Docker client
ENV DOCKER_ARCHIVE docker-19.03.3.tgz
ENV DOCKER_SRC_URL https://download.docker.com/linux/static/stable/x86_64/$DOCKER_ARCHIVE
RUN \
  curl -fsSLO "$DOCKER_SRC_URL" && \
  tar --strip-components=1 -xvzf "$DOCKER_ARCHIVE" -C /usr/local/bin

COPY files /
