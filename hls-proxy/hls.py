import os
import sys
import shutil
import subprocess

from urlparse import urljoin

SERVER_HOSTNAME         = os.environ['SERVER_HOSTNAME']
STREAMER_HOSTNAME       = os.environ['STREAMER_HOSTNAME']
OUTPUT_DIRECTORY        = os.environ['OUTPUT_DIRECTORY']
SERVER_PATH             = os.environ['SERVER_PATH']
OUTPUT_INDEX_FILE_NAME  = os.environ['OUTPUT_INDEX_FILE_NAME']
OUTPUT_TS_FILE_PATTERN  = os.environ['OUTPUT_TS_FILE_PATTERN']
SEGMENT_LEN             = 12 # Seconds

SOUT_CONFIG_TEMPLATE = '\
#std {{\
    access = livehttp {{\
        seglen = {seg_len},\
        delsegs = true,\
        numsegs = 10,\
        index = {index_path},\
        index-url = {ts_url}\
    }},\
    mux = ts{{\
        use-key-frames\
    }},\
    dst = {ts_path}\
}}'

def sout_config(local_root, server_root):
    raw = SOUT_CONFIG_TEMPLATE.format(
        seg_len=SEGMENT_LEN,
        index_path=os.path.join(local_root, OUTPUT_INDEX_FILE_NAME),
        ts_url=urljoin(server_root, OUTPUT_TS_FILE_PATTERN),
        ts_path=os.path.join(local_root, OUTPUT_TS_FILE_PATTERN)
    )
    # Removing whitespaces because of VLC's weird parsing rules
    return ''.join(raw.split())

VLC_COMMAND = lambda port, local_root, server_root: [
    'vlc',
    '-I', 'dummy',
    '--play-and-exit',
    '--live-caching', '300',
    'http://{}:{}'.format(STREAMER_HOSTNAME, port),
    '--sout', sout_config(local_root, server_root)
]

class Proxy:

    def __init__(self, port):
        self.process = None
        self.port = port

        subdir = '{}/'.format(self.port)
        self.local_root = os.path.join(OUTPUT_DIRECTORY, subdir)
        self.server_root = urljoin(
            'http://{}'.format(SERVER_HOSTNAME),
            os.path.join(SERVER_PATH, subdir)
        )

    def start(self):
        if os.path.exists(self.local_root):
            shutil.rmtree(self.local_root)
        os.makedirs(self.local_root)

        command = VLC_COMMAND(
            self.port,
            self.local_root,
            self.server_root
        )

        self.process = subprocess.Popen(
            command,
            stdout=sys.stderr.fileno(),
            stderr=sys.stderr.fileno(),
        )

    def alive(self):
        if self.process is None:
            return False
        return self.process.poll() is None

    def ready(self):
        index_file_path = os.path.join(self.local_root, OUTPUT_INDEX_FILE_NAME)
        return os.path.exists(index_file_path)

    def to_json(self):
        return {
            'id': self.port,
            'ready': self.ready(),
            'indexUrl': os.path.join(self.server_root, OUTPUT_INDEX_FILE_NAME)
        }

    def __del__(self):
        if self.alive():
            self.process.kill()
        if os.path.exists(self.local_root):
            shutil.rmtree(self.local_root)

    def __hash__(self):
        return hash(self.port)

    def __eq__(self, other):
        return self.port == other.port

    def __ne__(self, other):
        return not self.__eq__(other)
