#!/usr/bin/env python3

from streaming import Stream
from events import Event, EventNotifier

streams_by_id = {}
notifier = EventNotifier()

def poll_streams():
    dead_streams = [
        id for id, stream in streams_by_id.items()
        if not stream.alive()
    ]

    for id in dead_streams:
        del streams_by_id[id]

from flask import Flask, request, jsonify, Response

app = Flask(__name__)

from functools import wraps
from utils import validate_json_request, preprocess
from json import dumps

def with_stream(fn):
    @wraps(fn)

    def wrapped(stream_id, *args, **kwargs):
        try:
            stream = streams_by_id[stream_id]
            return fn(stream, *args, **kwargs)
        except KeyError:
            return jsonify(
                errors=['Stream with id {} does not exist'.format(stream_id)]
            ), 404

    return wrapped

@app.route('/streams', methods=['GET'])
@preprocess(poll_streams)
def streams():
    return Response(
        dumps([
            stream.to_json()
            for stream in streams_by_id.values()
        ]),
        mimetype='application/json'
    )

@app.route('/streams/<int:stream_id>', methods=['GET'])
@preprocess(poll_streams)
@with_stream
def stream(stream):
    return Response(
        dumps(stream.to_json()),
        mimetype='application/json'
    )

@app.route('/streams/<int:stream_id>', methods=['DELETE'])
@preprocess(poll_streams)
@with_stream
def unmonitor(stream):
    stream.unwatch()
    del streams_by_id[stream.id]

    return jsonify(
        status='OK'
    )

@app.route('/streams/<int:stream_id>/watch', methods=['POST'])
@preprocess(poll_streams)
@with_stream
def watch(stream):
    stream.watch()

    return Response(
        dumps(stream.to_json()),
        mimetype='application/json'
    )

@app.route('/streams/<int:stream_id>/unwatch', methods=['POST'])
@preprocess(poll_streams)
@with_stream
def unwatch(stream):
    stream.unwatch()

    return Response(
        dumps(stream.to_json()),
        mimetype='application/json'
    )

@app.route('/streams', methods=['POST'])
@validate_json_request('monitor')
@preprocess(poll_streams)
def monitor(payload):
    stream = Stream(
        payload['channel'],
        payload['quality']
    )

    if stream in streams_by_id.values():
        return jsonify(
            errors=['Already monitored']
        ), 400
    else:
        available = stream.is_available()
        if available:
            stream.monitor()
            streams_by_id[stream.id] = stream
            return Response(
                dumps(stream.to_json()),
                mimetype='application/json'
            )
        else:
            return jsonify(
                errors=['Channel is not available']
            ), 404

from ws import WebSocketsServer

if __name__ == '__main__':
    ws_server = WebSocketsServer(4242, notifier)
    ws_server.start_detached()

    app.run(
        host='0.0.0.0',
        port=80,
    )
