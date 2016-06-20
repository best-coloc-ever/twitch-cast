<stream-info>

  <span class="name">{ name }</span> <img class="icon joystick">
  <span class="game">{ game }</span>
  <br>
  <div>
    <img class="icon viewers"> { viewers } |
    <img class="icon views" style="margin-bottom: -2px"> { views } |
    <img class="icon follows" style="margin-bottom: -4px"> { followers }
  </div>

  <style scoped>
    .name {
      color: #8D8B96;
      font-weight: bold;
    }

    .game {
      color: #8D8B96;
      font-weight: bold;
    }

    .icon {
      height: 18px;
      width: 18px;
    }

    .joystick {
      content:url("https://discordapp.com/assets/536c2c45ade326ef4197eb48c75cff6a.svg");
    }

    .viewers {
      content:url("https://discordapp.com/assets/5c2bef02d6ffc10c89f544c32c04ed46.svg")
    }

    .views {
      content:url("https://discordapp.com/assets/ccf4c733929efd9762ab02cd65175377.svg")
    }

    .follows {
      content:url("https://discordapp.com/assets/dcbf6274f0ce0f393d064a72db2c8913.svg")
    }
  </style>

  <script>
    var REFRESH_INFO_INTERVAL = 30; // seconds
    var CLIENT_ID = '5sdczsrlv5d4mnx9j8ejrargx34oev4';

    var self = this;

    this.channel = null;
    this.viewers = '?';
    this.views = '?';
    this.followers = '?';

    function fetchStreamInfos() {
      if (self.channel)
        $.ajax({
          url: 'https://api.twitch.tv/kraken/streams/' + self.channel,
          headers: {
            'Client-ID': CLIENT_ID
          },
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
