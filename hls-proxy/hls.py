import os
import sys
import subprocess

SERVER_HOSTNAME         = os.environ['SERVER_HOSTNAME']
STREAMER_HOSTNAME       = os.environ['STREAMER_HOSTNAME']
OUTPUT_DIRECTORY        = os.environ['OUTPUT_DIRECTORY']
OUTPUT_INDEX_FILE_NAME  = os.environ['OUTPUT_INDEX_FILE_NAME']
OUTPUT_TS_FILE_PATTERN  = os.environ['OUTPUT_TS_FILE_PATTERN']
SEGMENT_LEN             = 12 # Seconds

SOUT_CONFIG_TEMPLATE = '\
#std {{\
    access = livehttp {{\
        seglen = {seg_len},\
        delsegs = true,\
        numsegs = 10,\
        index = {out_dir}/{idx_file},\
        index-url = http://{hostname}/video/{ts_pattern}\
    }},\
    mux = ts{{\
        use-key-frames\
    }},\
    dst = /data/video/{ts_pattern}\
}}'

def sout_config(subdir):
    raw = SOUT_CONFIG_TEMPLATE.format(
        seg_len=SEGMENT_LEN,
        out_dir=os.path.join(OUTPUT_DIRECTORY, subdir),
        idx_file=OUTPUT_INDEX_FILE_NAME,
        hostname=SERVER_HOSTNAME,
        ts_pattern=OUTPUT_TS_FILE_PATTERN
    )
    # Removing whitespaces because of VLC's weird parsing rules
    return ''.join(raw.split())

VLC_COMMAND = lambda port: [
    'vlc',
    '-I', 'dummy',
    '--play-and-exit',
    '--live-caching', '300',
    'http://{}:{}'.format(STREAMER_HOSTNAME, port),
    '--sout', sout_config('{}'.format(port))
]

class Proxy:

    def __init__(self, port):
        self.process = None
        self.port = port

    def start(self):
        self.process = subprocess.Popen(
            VLC_COMMAND(self.port),
            stdout=sys.stderr.fileno(),
            stderr=subprocess.PIPE,
        )

    def alive(self):
        if self.process is None:
            return False
        return self.process.poll() is None

    def __del__(self):
        if self.alive():
            self.process.kill()

    def __hash__(self):
        return hash(self.port)

    def __eq__(self, other):
        return self.port == other.port

    def __ne__(self, other):
        return not self.__eq__(other)
