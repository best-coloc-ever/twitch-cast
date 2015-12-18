import asyncio

from json import dumps

class Event:

    MONITORED   = 'monitored'
    UNMONITORED = 'unmonitored'
    WATCHED     = 'watched'
    UNWATCHED   = 'unwatched'
    READY       = 'ready'

    def __init__(self, type, data):
        self.data = data
        self.data['event'] = type

    def to_json(self):
        return dumps(self.data)

class EventNotifier:

    def __init__(self):
        self.queues = set()
        self.event_loop = None

    def set_event_loop(self, loop):
        self.event_loop = loop

    def send_event(self, event_type, **kwargs):
        event = Event(event_type, kwargs)

        asyncio.run_coroutine_threadsafe(
            self.send_event_async(event),
            self.event_loop
        )

    @asyncio.coroutine
    def send_event_async(self, event):
        for queue in self.queues:
            yield from queue.put(event)

    def new_queue(self):
        queue = asyncio.Queue()
        self.queues.add(queue)

        return queue

    def remove_queue(self, queue):
        self.queues.discard(queue)
