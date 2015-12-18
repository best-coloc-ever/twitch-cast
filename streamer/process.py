import sys
import subprocess

from threading import Thread

def start_process(command, on_exit):
    process = subprocess.Popen(
        command,
        stdout=sys.stderr.fileno(),
        stderr=sys.stderr.fileno(),
    )

    def run():
        process.wait()
        on_exit()

    thread = Thread(target=run)
    thread.start()

    return process
