#!/usr/bin/env bash

set -e

sender-run()   { ./scripts/dev run --rm sender-builder   $@ }
receiver-run() { ./scripts/dev run --rm receiver-builder $@ }

# Bundle the sender application
sender-run npm install
sender-run bower install
sender-run gulp dist

# Bundle the receiver application
receiver-run npm install
receiver-run webpack -p

# Magic
./scripts/prod up -d
