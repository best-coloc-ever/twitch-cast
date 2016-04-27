<stream-info>

  <span class="name">{ name }</span> 🕹
  <span class="game">{ game }</span>
  <br>
  <span>📺 { viewers } | 👀 { views } | ❤ { followers }</span>

  <style scoped>
    .name {
      color: #8D8B96;
      font-weight: bold;
    }

    .game {
      color: #8D8B96;
      font-weight: bold;
    }
  </style>

  <script>
    var REFRESH_INFO_INTERVAL = 30; // seconds

    var self = this;

    this.channel = null;
    this.viewers = '?';
    this.views = '?';
    this.followers = '?';

    function fetchStreamInfos() {
      if (self.channel)
        $.ajax({
          url: 'https://api.twitch.tv/kraken/streams/' + self.channel,
          success: function(data) {
            var stream = data.stream;
            var channel = stream.channel;

            self.viewers = stream.viewers.toLocaleString();
            self.views = channel.views.toLocaleString();
            self.followers = channel.followers.toLocaleString();
            self.game = channel.game;
            self.name = channel.display_name;

            self.update();
          }
        })
    }

    setChannel(channel) {
      this.channel = channel;
      fetchStreamInfos();
    }

    setInterval(fetchStreamInfos, REFRESH_INFO_INTERVAL * 1000);
  </script>
</stream-info>
