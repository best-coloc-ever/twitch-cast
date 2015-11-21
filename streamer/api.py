#!/usr/bin/env python

from streaming import Stream

monitored_streams = set()

def poll_streams():
    dead_streams = [
        stream for stream in monitored_streams
        if not stream.alive()
    ]

    for stream in dead_streams:
        monitored_streams.remove(stream)

from flask import Flask, request, jsonify

app = Flask(__name__)

from utils import validate_json_request, preprocess

@app.route('/streams', methods=['GET'])
@preprocess(poll_streams)
def streams():
    return jsonify(
        streams=[
            stream.to_json()
            for stream in monitored_streams
        ]
    )

@app.route('/streams', methods=['POST'])
@validate_json_request('monitor_stream')
@preprocess(poll_streams)
def monitor_stream(payload):
    url = 'twitch.tv/{}'.format(payload['channel'])
    stream = Stream(url, payload['quality'])

    if stream in monitored_streams:
        status = 'ALREADY_STARTED'
    else:
        available = stream.is_available()
        if available:
            status = 'OK'
            stream.start()
            monitored_streams.add(stream)
        else:
            status = 'UNAVAILABLE'

    return jsonify(
        status=status
    )

if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=80,
        debug=True
    )
