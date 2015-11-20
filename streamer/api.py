#!/usr/bin/env python

from flask import Flask, request, jsonify

app = Flask(__name__)

from streaming import Stream

monitored_streams = set()

def poll_streams():
    dead_streams = [
        stream for stream in monitored_streams
        if not stream.alive()
    ]

    for stream in dead_streams:
        monitored_streams.remove(stream)

@app.route('/streams', methods=['GET'])
def streams():
    poll_streams()

    return jsonify(
        streams=[
            stream.to_json()
            for stream in monitored_streams
        ]
    )

@app.route('/start', methods=['POST'])
def start_stream():
    channel = request.form['channel']
    quality = request.form['quality']

    poll_streams()

    url = 'twitch.tv/{}'.format(channel) # Twitch specific (for now ?)
    stream = Stream(url, quality)

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
