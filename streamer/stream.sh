#!/usr/bin/env bash

channel=$1

livestreamer \
    $channel \
    best \
    --player-external-http \
    --player-external-http-port 4242 \
    --yes-run-as-root
