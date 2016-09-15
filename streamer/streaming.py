import subprocess
import socket
import os

from hls import Proxy
from process import start_process
from livestreamer import Livestreamer

TWITCH_CLIENT_ID = os.environ['TWITCH_CLIENT_ID']
TWITCH_CLIENT_HTTP_HEADER = 'Client-ID={}'.format(TWITCH_CLIENT_ID)

LIVESTREAMER_COMMAND = lambda url, quality, port: [
    'livestreamer',
    url,
    quality,
    '--player-external-http',
    '--player-external-http-port', '{}'.format(port),
    '--http-header', TWITCH_CLIENT_HTTP_HEADER,
    '--yes-run-as-root'
]

# Asks the Kernel for a free port to bind
def bindable_port():
    sock = socket.socket(
        socket.AF_INET,
        socket.SOCK_STREAM
    )
    sock.bind(('', 0))
    _, port = sock.getsockname()
    sock.close()
    return port

class Stream:

    id_counter = 1

    def __init__(self, channel, quality):
        self.id = Stream.id_counter
        Stream.id_counter += 1

        self.channel = channel
        self.quality = quality
        self.url = 'twitch.tv/{}'.format(channel)
        self.port = bindable_port()
        self.process = None
        self.proxy = None
        self.on_exit = None

        self.session = Livestreamer()
        self.session.set_option('http-headers', TWITCH_CLIENT_HTTP_HEADER)

    def is_available(self):
        streams = self.session.streams(self.url)
        return self.quality in streams

    def monitor(self, on_exit):
        command = LIVESTREAMER_COMMAND(
            self.url,
            self.quality,
            self.port
        )

        def on_process_exit():
            self.process = None
            on_exit()

        self.process = start_process(command, on_process_exit)

        # Waiting for livestreamer's http server to accept requests
        subprocess.call([
            './scripts/wait_for_host.sh',
            '{}'.format(self.port)
        ])

    def watch(self, on_ready, on_exit):
        self.proxy = Proxy(self)

        def on_proxy_exit():
            self.unwatch()
            on_exit()

        self.proxy.start(on_ready, on_proxy_exit)

    def unwatch(self):
        if self.proxy:
            del self.proxy
            self.proxy = None

    def to_json(self):
        proxy = self.proxy.to_json() if self.proxy else None

        return {
            'id': self.id,
            'channel': self.channel,
            'quality': self.quality,
            'url': self.url,
            'proxy': proxy,
        }

    def __del__(self):
        self.unwatch()
        if self.process:
            self.process.kill()

    def __hash__(self):
        return hash((self.url, self.quality))

    def __eq__(self, other):
        return self.url == other.url and self.quality == other.quality

    def __ne__(self, other):
        return not self.__eq__(other)
