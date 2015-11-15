#!/usr/bin/env bash

if ! docker volume inspect twitch-cast-data &> /dev/null; then
    docker volume create twitch-cast-data
fi

docker-compose --x-networking up -d
