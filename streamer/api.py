#!/usr/bin/env python3

from streaming import Stream
streams_by_id = {}

from events import Event, EventNotifier
notifier = EventNotifier()

from flask import Flask, request, jsonify, Response
app = Flask(__name__)

from functools import wraps
from utils import validate_json_request
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

def remove_stream(stream_id):
    del streams_by_id[stream_id]

    notifier.send_event(
        Event.UNMONITORED,
        stream_id=stream_id
    )

@app.route('/streams', methods=['GET'])
def streams():
    return Response(
        dumps([
            stream.to_json()
            for stream in streams_by_id.values()
        ]),
        mimetype='application/json'
    )

@app.route('/streams/<int:stream_id>', methods=['GET'])
@with_stream
def stream(stream):
    return Response(
        dumps(stream.to_json()),
        mimetype='application/json'
    )

@app.route('/streams/<int:stream_id>', methods=['DELETE'])
@with_stream
def unmonitor(stream):
    stream.unwatch()
    del streams_by_id[stream.id]

    notifier.send_event(
        Event.UNMONITORED,
        stream_id=stream.id
    )
    return jsonify(
        status='OK'
    )

@app.route('/streams/<int:stream_id>/watch', methods=['POST'])
@with_stream
def watch(stream):
    stream.watch(
        on_ready=lambda: notifier.send_event(
            Event.READY,
            stream=stream.to_json()
        ),
        on_exit=lambda: notifier.send_event(
            Event.UNWATCHED,
            stream=stream.to_json()
        ),
    )

    notifier.send_event(
        Event.WATCHED,
        stream=stream.to_json()
    )
    return Response(
        dumps(stream.to_json()),
        mimetype='application/json'
    )

@app.route('/streams/<int:stream_id>/unwatch', methods=['POST'])
@with_stream
def unwatch(stream):
    stream.unwatch()

    notifier.send_event(
        Event.UNWATCHED,
        stream=stream.to_json()
    )
    return Response(
        dumps(stream.to_json()),
        mimetype='application/json'
    )

@app.route('/streams', methods=['POST'])
@validate_json_request('monitor')
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
            stream.monitor(on_exit=lambda: remove_stream(stream.id))
            streams_by_id[stream.id] = stream
            notifier.send_event(
                Event.MONITORED,
                stream=stream.to_json()
            )
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
