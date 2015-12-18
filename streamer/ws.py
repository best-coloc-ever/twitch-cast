import websockets
import asyncio

from threading import Thread

class WebSocketsServer:

    def __init__(self, port, notifier):
        self.port = port
        self.notifier = notifier

    @asyncio.coroutine
    def socket_handler(self, socket, path):
        queue = self.notifier.new_queue()

        while True:
            item = yield from queue.get()
            if not socket.open:
                break
            yield from socket.send(item.to_json())
            yield queue.task_done()

        self.notifier.remove_queue(queue)

    def run(self):
        server = websockets.serve(
            self.socket_handler,
            port=self.port
        )

        loop = asyncio.new_event_loop()

        self.notifier.set_event_loop(loop)
        asyncio.set_event_loop(loop)

        loop.run_until_complete(server)
        loop.run_forever()

    def start_detached(self):
        thread = Thread(target=self.run)
        thread.start()
