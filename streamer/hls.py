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
FFMPEG_DIST_DIR         = os.environ['FFMPEG_DIST_DIR']
SEGMENT_LEN             = 7 # Seconds
SEGMENT_COUNT           = 7

FFMPEG_COMMAND = lambda port, path: [
    '{}/bin/ffmpeg'.format(FFMPEG_DIST_DIR),
    '-i',                   'http://localhost:{}'.format(port),
    '-codec',               'copy',
    '-segment_list_flags',  '+live',
    '-hls_list_size',       str(SEGMENT_COUNT),
    '-hls_time',            str(SEGMENT_LEN),
    '-hls_flags',           'delete_segments',
    path
]

class Proxy:

    def __init__(self, stream):
        self.process = None
        self.stream = stream

        subdir = '{}/{}/'.format(stream.channel, stream.quality)
        self.local_root = os.path.join(OUTPUT_DIRECTORY, subdir)
        self.server_root = urljoin(
            'https://{}'.format(SERVER_HOSTNAME),
            os.path.join(SERVER_PATH, subdir)
        )

        self.index_path = os.path.join(self.local_root, OUTPUT_INDEX_FILE_NAME)
        self.index_url = os.path.join(self.server_root, OUTPUT_INDEX_FILE_NAME)

        self.ready = False

    def start(self, on_ready, on_exit):
        self.cleanup_local_data()
        os.makedirs(self.local_root)

        command = FFMPEG_COMMAND(
            self.stream.port,
            self.index_path,
        )

        self.process = start_process(command, on_exit)
        self.poll_until_ready(on_ready)

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
