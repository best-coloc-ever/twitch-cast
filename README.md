# twitch-cast [![Build Status](https://travis-ci.org/best-coloc-ever/twitch-cast.svg?branch=master)](https://travis-ci.org/best-coloc-ever/twitch-cast)
Cast twitch on your Chromecast without random quality drops

## Deployment
Make sure you have **docker >= 1.10** and **docker-compose >= 1.6** installed 

Generate your SSL/TLS certificate in `frontend/server/fullchain.pem`  
Place the associated private key in `frontend/server/privkey.pem`

then run:
```sh
SERVER_NAME=your_domain_or_ip ./deploy.sh
```
