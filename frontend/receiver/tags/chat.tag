<chat>

  <!-- Layout -->
  <ul id="chat">
    <!-- <chat-line each={ message in messages } message={ message } store={ parent.store }>
    </chat-line> -->
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
    var CHAT_DISPLAY_INTERVAL = 0.3; // seconds
    var CHAT_CLEAR_INTERVAL = 30; // seconds
    var CHAT_DELAY = 42; // seconds

    var self = this;
    var messageQueue = [];
    var ws = null;
    var retryTimeout = null;

    this.messages = [];

    notify(text) {
      self.addMessage({ sender: 'SYSTEM', content: text });
      self.update();
    }

    onmessage(e) {
      var message = JSON.parse(e.data);
      message.stamp = new Date().getTime();
      messageQueue.push(message);
    }

    pause() {
      self.notify('Pausing chat');
      ws.onmessage = null;
    }

    resume() {
      self.notify('Resuming chat');
      ws.onmessage = self.onmessage;
    }

    function connectToChat(channel) {
      clearTimeout(retryTimeout);
      if (ws)
        ws.close();

      self.notify('Joining channel: ' + channel);

      var url = 'ws://' + window.location.host + '/chat/' + channel;
      ws = new WebSocket(url);

      ws.onopen = function(e) {
        self.notify('Successfully joined the channel!');
        self.notify('Delaying chat for ' + CHAT_DELAY + ' seconds');
      };

      ws.onclose = function(e) {
        if (e.code != 1000)
          retryTimeout = setTimeout(function() { connectToChat(channel); }, 2000);
      }

      ws.onerror = function(e) {
        self.notify('Error: ' + e.reason);
      };

      ws.onmessage = self.onmessage;
    }

    function processMessageQueue() {
      var i = 0;
      var now = new Date().getTime();

      for (; i < messageQueue.length; ++i) {
        var message = messageQueue[i];

        if (now - message.stamp < CHAT_DELAY * 1000)
          break ;

        self.addMessage(message);
      }

      messageQueue = messageQueue.splice(0, i + 1);

      self.update();
      self.root.scrollTop = self.root.scrollHeight;

      setTimeout(processMessageQueue, CHAT_DISPLAY_INTERVAL * 1000);
    }

    // function limitLines() {
    //   var toSlice = Math.max(0, self.messages.length - CHAT_MESSAGE_MAX_COUNT);
    //   self.messages = self.messages.slice(toSlice);

    //   self.update();

    //   setTimeout(limitLines, CHAT_CLEAR_INTERVAL * 1000);
    // }

    setChannel(channel) {
      self.ul = $(this.root).find('#chat');

      messageQueue = [];
      this.messages = [];
      this.update();

      this.store = new ChatAssetStore(channel);
      connectToChat(channel);
    }

    addMessage(message) {
      self.messages.push(message);

      // JQuery
      var chatLine = buildChatLine(message, self.store);
      self.ul.append(chatLine);

      var toSlice = Math.max(0, self.messages.length - CHAT_MESSAGE_MAX_COUNT);
      self.messages = self.messages.slice(toSlice);

      self.ul.find('li:lt(' + toSlice + ')').remove();
    }

    this.on('mount', function() {
      self.ul = $(this.root).find('#chat');

      processMessageQueue();
    })

  </script>

</chat>
