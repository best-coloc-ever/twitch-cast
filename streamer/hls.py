import os
import shutil

from urllib.parse import urljoin
from time import sleep

from process import start_process
from threading import Thread

SERVER_HOSTNAME         = os.environ['SERVER_HOSTNAME']
STREAMER_HOSTNAME       = os.environ['STREAMER_HOSTNAME']
OUTPUT_DIRECTORY        = os.environ['OUTPUT_DIRECTORY']
SERVER_PATH             = os.environ['SERVER_PATH']
OUTPUT_INDEX_FILE_NAME  = os.environ['OUTPUT_INDEX_FILE_NAME']
OUTPUT_TS_FILE_PATTERN  = os.environ['OUTPUT_TS_FILE_PATTERN']
SEGMENT_LEN             = 5 # Seconds
SEGMENT_COUNT           = 5

SOUT_CONFIG_TEMPLATE = '\
#std {{\
    access = livehttp {{\
        seglen = {seg_len},\
        delsegs = true,\
        numsegs = {seg_count},\
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

        self.ready = False

    def start(self, on_ready, on_exit):
        self.cleanup_local_data()
        os.makedirs(self.local_root)

        command = VLC_COMMAND(
            self.stream.port,
            self.sout_config()
        )

        self.process = start_process(command, on_exit)
        self.poll_until_ready(on_ready)

    def sout_config(self):
        raw = SOUT_CONFIG_TEMPLATE.format(
            seg_len=SEGMENT_LEN,
            seg_count=SEGMENT_COUNT,
            index_path=self.index_path,
            ts_url=urljoin(self.server_root, OUTPUT_TS_FILE_PATTERN),
            ts_path=os.path.join(self.local_root, OUTPUT_TS_FILE_PATTERN)
        )
        # Removing whitespaces because of VLC's weird parsing rules
        return ''.join(raw.split())

    def to_json(self):
        return {
            'ready': self.ready,
            'indexUrl': self.index_url,
        }

    def cleanup_local_data(self):
        if os.path.exists(self.local_root):
            shutil.rmtree(self.local_root)

    def poll_until_ready(self, on_ready):

        def run():
            while not os.path.exists(self.index_path):
                sleep(0.25)
            self.ready = True
            on_ready()

        thread = Thread(target=run)
        thread.start()

    def __del__(self):
        if self.process:
            self.process.kill()
        self.cleanup_local_data()
