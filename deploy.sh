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
receiver-run npm install
receiver-run webpack -p

# Bundle the receiver application
receiver-run npm install
receiver-run webpack -p

# Build the streamer app
streamer-run stack --allow-different-user build --copy-bins --local-bin-path /dist

# Magic
./scripts/prod up -d
