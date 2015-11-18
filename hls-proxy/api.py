#!/usr/bin/env python

from flask import Flask, request, jsonify

app = Flask(__name__)

import hls
import os

hls_proxy = hls.Proxy(
    os.environ['SERVER_HOSTNAME'],
    os.environ['STREAMER_HOSTNAME'],
    os.environ['OUTPUT_DIRECTORY'],
    os.environ['OUTPUT_INDEX_FILE_NAME'],
    os.environ['OUTPUT_TS_FILE_PATTERN'],
)

@app.route('/watch', methods=['POST'])
def watch():
    port = request.form['port']

    hls_proxy.start(port)

    return jsonify(
        status='OK'
    )

if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=80,
        debug=True
    )
