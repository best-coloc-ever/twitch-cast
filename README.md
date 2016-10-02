# twitch-cast [![Build Status](https://travis-ci.org/best-coloc-ever/twitch-cast.svg?branch=master)](https://travis-ci.org/best-coloc-ever/twitch-cast)

A Chromecast application and proxy for [twitch.tv](https://twitch.tv)

official web application available at [datcoloc.com/twitch-cast](https://datcoloc.com/twitch-cast)

## Deployment
Make sure you have **docker >= 1.10** and **docker-compose >= 1.6** installed 

Generate your SSL/TLS certificate in `frontend/server/fullchain.pem`  
Place the associated private key in `frontend/server/privkey.pem`

then run:
```sh
SERVER_NAME=your_domain_or_ip \
TWITCH_CLIENT_ID=your_twitch_client_id \
./deploy.sh
```
