#!/usr/bin/env python

from streaming import Stream

monitored_streams = dict()

def poll_streams():
    dead_streams = [
        id for id, stream in monitored_streams.items()
        if not stream.alive()
    ]

    for id in dead_streams:
        del monitored_streams[id]

from flask import Flask, request, jsonify

app = Flask(__name__)

from utils import validate_json_request, preprocess

@app.route('/streams', methods=['GET'])
@preprocess(poll_streams)
def streams():
    return jsonify(
        streams=[
            stream.to_json()
            for stream in monitored_streams.values()
        ]
    )

@app.route('/stream/<int:stream_id>', methods=['GET'])
@preprocess(poll_streams)
def stream(stream_id):
    try:
        stream = monitored_streams[stream_id]
    except KeyError:
        return jsonify(
            errors=['stream with id {} does not exist'.format(stream_id)]
        ), 404

    return jsonify(
        stream=stream.to_json()
    )

@app.route('/streams', methods=['POST'])
@validate_json_request('monitor_stream')
@preprocess(poll_streams)
def monitor_stream(payload):
    url = 'twitch.tv/{}'.format(payload['channel'])
    stream = Stream(url, payload['quality'])

    if stream in monitored_streams.values():
        status = 'ALREADY_STARTED'
    else:
        available = stream.is_available()
        if available:
            status = 'OK'
            stream.start()
            monitored_streams[stream.port] = stream
        else:
            status = 'UNAVAILABLE'

    return jsonify(
        status=status
    )

@app.route('/stream/<int:stream_id>', methods=['DELETE'])
@preprocess(poll_streams)
def unmonitor_stream(stream_id):
    try:
        del monitored_streams[stream_id]
    except KeyError:
        return jsonify(
            errors=['stream with id {} does not exist'.format(stream_id)]
        ), 404

    return jsonify(
        status='OK'
    )

if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=80,
        debug=True
    )
