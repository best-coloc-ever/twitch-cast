#!/usr/bin/env bash

if ! docker inspect twitch-cast-frontend-builder &> /dev/null; then
  docker build -t twitch-cast-frontend-builder frontend/chromecast/sender/
fi

docker run --rm -v $PWD/frontend/chromecast/sender/:/src twitch-cast-frontend-builder gulp dist

if ! docker volume inspect twitch-cast-data &> /dev/null; then
    docker volume create --name twitch-cast-data
fi

docker-compose --x-networking up -d
