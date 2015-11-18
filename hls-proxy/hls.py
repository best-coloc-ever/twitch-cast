import sys
import subprocess

VLC_COMMAND = lambda in_stream, sout_config: [
    'vlc',
    '-I', 'dummy',
    '--play-and-exit',
    '--live-caching', '300',
    in_stream,
    '--sout', sout_config
]

class Proxy:

    SEGMENT_LEN = 12

    def __init__(self, hostname, streamer_hostname, out_dir, idx_file, ts_pattern):
        self.hostname = hostname
        self.streamer_hostname = streamer_hostname
        self.out_dir = out_dir
        self.idx_file = idx_file
        self.ts_pattern = ts_pattern
        self.process = None

    def start(self, port):
        if self.process is not None:
            self.process.kill()

        in_stream = 'http://{}:{}'.format(self.streamer_hostname, port)
        command = VLC_COMMAND(
            in_stream,
            self.sout_config()
        )

        self.process = subprocess.Popen(
            command,
            stdout=sys.stderr.fileno(),
            stderr=subprocess.PIPE,
        )

    def sout_config(self):
        raw = '#std {{\
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
        }}'.format(
            seg_len=Proxy.SEGMENT_LEN,
            out_dir=self.out_dir,
            idx_file=self.idx_file,
            hostname=self.hostname,
            ts_pattern=self.ts_pattern
        )
        # Removing whitespaces because of VLC's weird parsing rules
        return ''.join(raw.split())
