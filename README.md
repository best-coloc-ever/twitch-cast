# twitch-cast
Cast twitch on your Chromecast

## Deployment
With docker:
```sh
docker build -t twitch-castd . # Builds the main server image
docker build -t streamer -f streamer.Dockerfile . # Builds an image for a sandboxed livestreamer
docker build -t vlc -f vlc.Dockerfile . # Builds an image for a sandboxed vlc

mkdir video
chmod 777 video

docker network create streaming

docker run -d --name twitch-castd -v $PWD/chromecast:/chromecast -v $PWD/video:/data/video -p 80:80 twitch-cast
docker run -d --name streamer --net streaming livestreamer twitch.tv/cohhcarnage best --player-external-http --player-external-http-port 4242 --yes-run-as-root
docker run --name vlc --net streaming -v $PWD/video:/video streamer vlc \
    -I dummy \
    http://streamer:4242 \
    --sout '#duplicate{dst=std{access=livehttp{seglen=12,delsegs=true,numsegs=10,index=/video/stream.m3u8,index-url=http://178.62.170.231/video/stream-########.ts},mux=ts{use-key-frames},dst=/video/stream-########.ts},dst=std{access=http,mux=ts,dst=:8082/video.mp4}}' \
    --live-caching=2000
```
