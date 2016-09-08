#! /usr/bin/env bash

browser-sync start \
    --proxy https://web/chromecast \
    --files /dist \
    --no-open \
    --no-notify \
    &

webpack -p --watch
