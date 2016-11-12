#! /usr/bin/env bash

browser-sync start \
    --proxy https://web/twitch-cast \
    --files /dist \
    --no-open \
    --no-notify \
    &

webpack -p --watch
