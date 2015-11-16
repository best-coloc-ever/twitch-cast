#!/usr/bin/env bash

url=$1
log_file="/livestreamer.log"

livestreamer \
    $url \
    best \
    --player-external-http \
    --player-external-http-port 4242 \
    --yes-run-as-root &> $log_file
