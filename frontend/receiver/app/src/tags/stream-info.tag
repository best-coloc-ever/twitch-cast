<stream-info>

  <span class="name">{ name }</span> <img class="icon joystick">
  <span class="game">{ game }</span>
  <br>
  <div>
    <img class="icon viewers"> { viewers }
    <img class="icon views"> { views }
    <img class="icon follows"> { followers }
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
      content:url("/twitch-cast/img/icons/joystick.svg");
    }

    .viewers {
      content:url("/twitch-cast/img/icons/viewers.svg");
    }

    .views {
      content:url("/twitch-cast/img/icons/views.svg");
      margin-left: 10px;
    }

    .follows {
      content:url("/twitch-cast/img/icons/follows.svg");
      margin-left: 10px;
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

    this.setChannel = (channel) => {
      this.channel = channel;
      fetchStreamInfos();
    }

    setInterval(fetchStreamInfos, REFRESH_INFO_INTERVAL * 1000);
  </script>
</stream-info>
