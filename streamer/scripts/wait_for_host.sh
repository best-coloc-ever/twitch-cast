#!/usr/bin/env bash

port=$1

while ! echo exit | nc localhost $port &> /dev/null; do
    sleep .5
done
