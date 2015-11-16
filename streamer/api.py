#!/usr/bin/env python

from flask import Flask, request

app = Flask(__name__)

import os
import subprocess
from signal import SIGTERM

streamer_process = None
def start_stream(url):
    global streamer_process

    if streamer_process is not None:
        os.killpg(streamer_process.pid, SIGTERM)

    # TODO: check if sucessful
    streamer_process = subprocess.Popen(
        ['/stream.sh', url],
        stdout=subprocess.PIPE,
        preexec_fn=os.setsid,
    )

@app.route('/stream', methods=['POST'])
def stream():
    channel = request.form['channel']

    url = 'twitch.tv/{}'.format(channel) # Twitch specific (for now ?)
    start_stream(url)

    return 'OK'

if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=80,
    )
