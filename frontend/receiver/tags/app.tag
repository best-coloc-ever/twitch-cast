<app>

  <!-- Layout -->
  <div class="box">

    <stream class="flex"></stream>

    <chat if={ chatVisible }></chat>
  </div>

  <!-- Style -->
  <style scoped>
    .box {
      display: flex;
      height: 100%;
    }

    .flex {
      flex: 1 1 auto;
      height: 100%;
      position: relative;
    }

  </style>

  <!-- Logic -->
  <script>
    var self = this;

    this.chatVisible = true;
    this.chatPosition = true;
    this.desktop = (navigator.userAgent.indexOf('CrKey') == -1);

    initChromecast() {
      var stream = this.tags.stream;
      var chat = this.tags.chat;

      var receiver = new TwitchCastReceiver(stream.mediaElement());

      receiver.on('channel', function(e) {
        chat.setChannel(e.channel);
        stream.setChannel(e.channel);
      });

      receiver.on('chatToggle', function(e) {
        self.chatVisible = e.visible;
        stream.fullScreen(!e.visible);

        if (e.visible)
          chat.resume();
        else
          chat.pause();

        self.update();
      });

      receiver.on('chatPosition', function(e) {
        self.chatPosition = !self.chatPosition;
        if (self.chatPosition)
          $(chat.root).insertAfter($(stream.root));
        else
          $(chat.root).insertBefore($(stream.root));
      });

      receiver.on('notice', function(e) {
        stream.notice(e);
      });

    }

    initDesktop() {
      var id = riot.route.query().id;

      if (id) {
        $.get(
          '/streamer/streams/' + id,
          function(data) {
            self.tags.chat.setChannel(data.channel);
            self.tags.stream.setChannel(data.channel);
            self.tags.stream.setDesktopSource(data.proxy.indexUrl);
          }
        );
      }
    }

    this.on('mount', function() {
      if (this.desktop)
        this.initDesktop();
      else
        this.initChromecast();
    });

  </script>

</app>
