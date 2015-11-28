#!/usr/bin/env bash

set -eu

dc() {
    docker-compose --x-networking "$@"
}

# Data volumes
docker volume create --name "twitch-cast-data"         # Video storage
docker volume create --name "twitch-cast-sender-dist"  # Prod website storage
docker volume create --name "twitch-cast-sender-dev"   # Dev website storage
docker volume create --name "twitch-cast-node-modules" # Node package cache

# Asset pipeline
dc run --rm frontend-builder npm install
dc run --rm frontend-builder bower install --allow-root
dc run --rm frontend-builder gulp dist

# Magic
dc up -d
