#!/usr/bin/env python

from flask import Flask, request, jsonify

app = Flask(__name__)

import hls
import os

hls_proxies = set()

def poll_proxies():
    dead_proxies = [
        proxy for proxy in hls_proxies
        if not proxy.alive()
    ]

    for proxy in dead_proxies:
        hls_proxies.remove(proxy)

@app.route('/proxies', methods=['GET'])
def proxies():
    poll_proxies()

    return jsonify(
        proxies=[
            proxy.to_json()
            for proxy in hls_proxies
        ]
    )

@app.route('/watch', methods=['POST'])
def watch():
    port = request.form['port']

    poll_proxies()

    proxy = hls.Proxy(port)
    if proxy in hls_proxies:
        status = 'ALREADY_WATCHING'
    else:
        proxy.start()
        hls_proxies.add(proxy)
        status = 'OK'

    return jsonify(
        status=status
    )

@app.route('/unwatch', methods=['POST'])
def unwatch():
    port = request.form['port']

    proxy = hls.Proxy(port)
    if proxy in hls_proxies:
        hls_proxies.remove(proxy)
        status = 'OK'
    else:
        status = 'NOT_FOUND'

    return jsonify(
        status=status
    )

if __name__ == '__main__':
    app.run(
        host='0.0.0.0',
        port=80,
        debug=True
    )
