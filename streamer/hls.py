import os
import sys
import shutil
import subprocess

from urllib.parse import urljoin

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

VLC_COMMAND = lambda port, sout_config: [
    'vlc',
    '-I', 'dummy',
    '--play-and-exit',
    '--live-caching', '300',
    'http://{}:{}'.format(STREAMER_HOSTNAME, port),
    '--sout', sout_config
]

class Proxy:

    def __init__(self, stream):
        self.process = None
        self.stream = stream

        subdir = '{}/{}/'.format(stream.channel, stream.quality)
        self.local_root = os.path.join(OUTPUT_DIRECTORY, subdir)
        self.server_root = urljoin(
            'http://{}'.format(SERVER_HOSTNAME),
            os.path.join(SERVER_PATH, subdir)
        )

        self.index_path = os.path.join(self.local_root, OUTPUT_INDEX_FILE_NAME)
        self.index_url = os.path.join(self.server_root, OUTPUT_INDEX_FILE_NAME)

    def start(self):
        self.cleanup_local_data()
        os.makedirs(self.local_root)

        command = VLC_COMMAND(
            self.stream.port,
            self.sout_config()
        )

        self.process = subprocess.Popen(
            command,
            stdout=sys.stderr.fileno(),
            stderr=sys.stderr.fileno(),
        )

    def sout_config(self):
        raw = SOUT_CONFIG_TEMPLATE.format(
            seg_len=SEGMENT_LEN,
            index_path=self.index_path,
            ts_url=urljoin(self.server_root, OUTPUT_TS_FILE_PATTERN),
            ts_path=os.path.join(self.local_root, OUTPUT_TS_FILE_PATTERN)
        )
        # Removing whitespaces because of VLC's weird parsing rules
        return ''.join(raw.split())

    def alive(self):
        if self.process is None:
            return False
        return self.process.poll() is None

    def ready(self):
        return os.path.exists(self.index_path)

    def to_json(self):
        return {
            'ready': self.ready(),
            'indexUrl': self.index_url,
        }

    def cleanup_local_data(self):
        if os.path.exists(self.local_root):
            shutil.rmtree(self.local_root)

    def __del__(self):
        if self.alive():
            self.process.kill()
        self.cleanup_local_data()
