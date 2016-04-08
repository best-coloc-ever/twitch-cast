<chat>

  <!-- Layout -->
  <ul>
    <li each={ notice in notices }>{ notice }</li>
    <chat-line each={ message in messages } message={ message } store={ parent.store }>
    </chat-line>
  </ul>

  <!-- Style -->
  <style scoped>
    :scope {
      flex: 0 1 250px;
      min-width: 250px;
      background: #19191f;
      overflow-y: hidden;
      overflow-x: hidden;
      word-wrap: break-word;
    }

    ul {
      padding-left: 10px;
    }

    li {
      color: white;
    }
  </style>

  <!-- Logic -->
  <script>
    var CHAT_MESSAGE_MAX_COUNT = 50;
    var CHAT_DISPLAY_INTERVAL = 0.25; // seconds
    var CHAT_CLEAR_INTERVAL = 10; // seconds
    var CHAT_DELAY = 42; // seconds

    var self = this;
    var messageQueue = [];
    var ws = null;

    this.notices = [];
    this.messages = [];

    notify(text) {
      self.notices.push(text);
      self.update();
    }

    function connectToChat(channel) {
      self.notify('Joining channel: ' + channel);

      if (ws)
        ws.close();

      var url = 'ws://' + window.location.host + '/chat/' + channel;
      ws = new WebSocket(url);

      ws.onopen = function(e) {
        self.notify('Successfully joined the channel!');
        self.notify('Delaying chat for ' + CHAT_DELAY + ' seconds');
      };

      ws.onclose = function(e) {
        if (e.code != 1000)
          setTimeout(connectToChat, 2000);
      }

      ws.onerror = function(e) {
        self.notify('Error: ' + e.reason);
      };

      ws.onmessage = function(e) {
        var message = JSON.parse(e.data);
        message.stamp = new Date().getTime();
        messageQueue.push(message);
      };
    }

    function processMessageQueue() {
      var i = 0;
      var now = new Date().getTime();

      for (; i < messageQueue.length; ++i) {
        var message = messageQueue[i];

        if (now - message.stamp < CHAT_DELAY * 1000)
          break ;

        self.messages.push(message);
      }

      messageQueue = messageQueue.slice(i, messageQueue.length);

      self.update();
      self.root.scrollTop = self.root.scrollHeight;

      setTimeout(processMessageQueue, CHAT_DISPLAY_INTERVAL * 1000);
    }

    function limitLines() {
      var toSlice = Math.max(0, self.messages.length - CHAT_MESSAGE_MAX_COUNT);
      self.messages = self.messages.slice(toSlice);

      self.update();

      setTimeout(limitLines, CHAT_CLEAR_INTERVAL * 1000);
    }

    setChannel(channel) {
      messageQueue = [];
      this.messages = [];
      this.notices = [];
      this.update();

      this.store = new ChatAssetStore(channel);
      connectToChat(channel);
    }

    processMessageQueue();
    limitLines();

  </script>

</chat>
