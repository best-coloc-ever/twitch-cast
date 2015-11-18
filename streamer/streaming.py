import sys
import subprocess
import socket
import livestreamer

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

    def __init__(self, url, quality):
        self.url = url
        self.quality = quality
        self.port = bindable_port()
        self.process = None

    def is_available(self):
        streams = livestreamer.streams(self.url)
        return streams.has_key(self.quality)

    def start(self):
        command = LIVESTREAMER_COMMAND(
            self.url,
            self.quality,
            self.port
        )

        self.process = subprocess.Popen(
            command,
            stdout=sys.stderr.fileno(),
            stderr=subprocess.PIPE,
        )

    def alive(self):
        if self.process is None:
            return False
        return self.process.poll() is None

    def to_json(self):
        return {
            'url': self.url,
            'quality': self.quality,
            'port': self.port
        }

    def __del__(self):
        if self.alive():
            self.process.kill()

    def __hash__(self):
        return hash((self.url, self.quality))

    def __eq__(self, other):
        return self.url == other.url and self.quality == other.quality

    def __ne__(self, other):
        return not self.__eq__(other)
