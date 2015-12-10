import sys
import subprocess
import socket
import livestreamer

from hls import Proxy

LIVESTREAMER_COMMAND = lambda url, quality, port: [
    'livestreamer',
    url,
    quality,
    '--player-external-http',
    '--player-external-http-port', '{}'.format(port),
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

    def is_available(self):
        streams = livestreamer.streams(self.url)
        return streams.has_key(self.quality)

    def monitor(self):
        command = LIVESTREAMER_COMMAND(
            self.url,
            self.quality,
            self.port
        )

        self.process = subprocess.Popen(
            command,
            stdout=sys.stderr.fileno(),
            stderr=sys.stderr.fileno(),
        )

        # Waiting for livestreamer's http server to accept requests
        subprocess.call([
            './scripts/wait_for_host.sh',
            '{}'.format(self.port)
        ])

    def watch(self):
        self.proxy = Proxy(self)
        self.proxy.start()

    def unwatch(self):
        if self.proxy:
            del self.proxy
            self.proxy = None

    def alive(self):
        if self.proxy and not self.proxy.alive():
            self.unwatch()
        if self.process is None:
            return False
        return self.process.poll() is None

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
        if self.alive():
            self.process.kill()

    def __hash__(self):
        return hash((self.url, self.quality))

    def __eq__(self, other):
        return self.url == other.url and self.quality == other.quality

    def __ne__(self, other):
        return not self.__eq__(other)
