#!/usr/bin/env python

from hls import Proxy

hls_proxies = dict()

def poll_proxies():
    dead_proxies = [
        id for id, proxy in hls_proxies.items()
        if not proxy.alive()
    ]

    for id in dead_proxies:
        del hls_proxies[id]

from flask import Response, Flask, request, jsonify
from json import dumps

app = Flask(__name__)

from utils import validate_json_request, preprocess

@app.route('/proxies', methods=['GET'])
@preprocess(poll_proxies)
def proxies():
    return Response(
        dumps([
            proxy.to_json()
            for proxy in hls_proxies.values()
        ]),
        mimetype='application/json'
    )

@app.route('/proxies/<int:proxy_id>', methods=['GET'])
@preprocess(poll_proxies)
def proxy(proxy_id):
    try:
        proxy = hls_proxies[proxy_id]
    except KeyError:
        return jsonify(
            errors=['proxy with id {} does not exist'.format(proxy_id)]
        ), 404

    return Response(dumps(proxy.to_json()), mimetype='application/json')

@app.route('/proxies', methods=['POST'])
@validate_json_request('watch')
@preprocess(poll_proxies)
def watch(payload):
    stream_id = payload['stream_id']

    try:
        proxy = hls_proxies[stream_id]
        return '', 400
    except KeyError:
        proxy = Proxy(stream_id)
        proxy.start()
        hls_proxies[stream_id] = proxy
        return Response(dumps(proxy.to_json()), mimetype='application/json')

@app.route('/proxies/<int:proxy_id>', methods=['DELETE'])
def unwatch(proxy_id):
    try:
        del hls_proxies[proxy_id]
    except KeyError:
        return jsonify(
            errors=['proxy with id {} does not exist'.format(proxy_id)]
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
