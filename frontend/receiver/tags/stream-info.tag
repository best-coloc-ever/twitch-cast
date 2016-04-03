<stream-info>

  <span>📺 { viewers }   /   👀 { views }   /   ❤ { followers }</span>

  <style scoped>
    span {
      color: white;
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
            self.viewers = data.stream.viewers.toLocaleString();
            self.views = data.stream.channel.views.toLocaleString();
            self.followers = data.stream.channel.followers.toLocaleString();
          }
        })
    }

    setChannel(channel) {
      this.channel = channel;
    }

    setInterval(fetchStreamInfos, REFRESH_INFO_INTERVAL * 1000);
  </script>
</stream-info>
