#! /usr/bin/env bash

browser-sync start \
    --proxy https://web/twitch-cast \
    --port 3001 \
    --files /dist \
    --no-open \
    --no-notify \
    &

webpack -p --watch
