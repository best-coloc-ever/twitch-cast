<chat>

  <!-- Layout -->
  <ul id="chat" name="chat">
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
    import ChatAssetStore from 'chat_assets.js'
    import buildChatLine from 'chat_line.js'
    import ReceiverEvent from 'receiver/events.js'

    const CHAT_MESSAGE_MAX_COUNT = 50;
    const CHAT_DISPLAY_INTERVAL = 0.3; // seconds
    const CHAT_CLEAR_INTERVAL = 30; // seconds
    const CHAT_DELAY = 12; // seconds

    var self = this;
    var messageQueue = [];
    var ws = null;
    var retryTimeout = null;

    this.messages = [];

    this.notify = (text) => {
      self.addMessage({ sender: 'SYSTEM', content: text });
      self.update();
    }

    this.onmessage = (e) => {
      var message = JSON.parse(e.data);
      message.stamp = new Date().getTime();
      messageQueue.push(message);
    }

    this.pause = () => {
      self.notify('Pausing chat');
      ws.onmessage = null;
    }

    this.resume = () => {
      self.notify('Resuming chat');
      ws.onmessage = self.onmessage;
    }

    function connectToChat(channel) {
      clearTimeout(retryTimeout);
      if (ws)
        ws.close();

      self.notify('Joining channel: ' + channel);

      var url = 'wss://' + window.location.host + '/chat/' + channel;
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

      messageQueue.splice(0, i);

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

    this.setChannel = (channel) => {
      messageQueue = [];
      this.messages = [];
      this.update();

      this.store = new ChatAssetStore(channel);
      connectToChat(channel);
    }

    this.addMessage = (message) => {
      self.messages.push(message);

      // JQuery
      var chatLine = buildChatLine(message, self.store);
      $(self.chat).append(chatLine);

      var toSlice = Math.max(0, self.messages.length - CHAT_MESSAGE_MAX_COUNT);
      self.messages = self.messages.slice(toSlice);

      $(self.chat).find('li:lt(' + toSlice + ')').remove();
    }

    this.on('mount', function() {
      let self = this,
          receiver = this.parent.receiver

      receiver.on(ReceiverEvent.ChannelChanged, e => {
        self.setChannel(e.channel)
      })

      receiver.on(ReceiverEvent.ChatToggled, e => {
        if (e.visible)
          self.resume()
        else
          self.pause()
      })

      processMessageQueue()
    })

  </script>

</chat>
