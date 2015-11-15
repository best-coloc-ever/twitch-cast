#!/usr/bin/env bash

output_directory="/data/video"
output_playlist_file_name="stream.m3u8"
output_ts_file_pattern="stream-#####.ts"

mkdir -p $output_directory # Ensure output directory exist

input_stream='http://streamer:4242'
sout_config_pretty="\
#std {\
    access = livehttp {\
        seglen = 12,\
        delsegs = true,\
        numsegs = 10,\
        index = $output_directory/$output_playlist_file_name,\
        index-url = http://178.62.170.231/video/$output_ts_file_pattern\
    },\
    mux = ts{\
        use-key-frames\
    },\
    dst = /data/video/$output_ts_file_pattern\
}"
# Forced to remove whitespaces because of VLC's weird parsing rules
sout_config="${sout_config_pretty//[[:blank:]]/}"

vlc \
    -I dummy \
    --play-and-exit \
    $input_stream \
    --live-caching 300 \
    --sout "$sout_config"
