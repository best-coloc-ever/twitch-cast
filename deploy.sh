#!/usr/bin/env bash

set -e

# Asset pipeline
docker-compose run --rm sender-builder npm install
docker-compose run --rm sender-builder bower install
docker-compose run --rm sender-builder gulp dist

# Magic
docker-compose up -d
