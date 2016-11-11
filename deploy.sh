#!/usr/bin/env bash

set -e

sender-run()   {
  ./scripts/dev run --rm sender-builder   $@
}

receiver-run() {
  ./scripts/dev run --rm receiver-builder $@
}

streamer-run() {
  ./scripts/dev run --rm streamer-builder $@
}

# Bundle the sender application
sender-run npm install
sender-run bower install
sender-run gulp dist

# Bundle the receiver application
receiver-run npm install
receiver-run webpack -p

# Build the streamer app
streamer-run stack build --copy-bins --local-bin-path /dist

# Magic
./scripts/prod up -d
