<stream-info>

  <span class="name">{ name }</span> <img class="icon joystick">
  <span class="game">{ game }</span>
  <br>
  <div>
    <img class="icon viewers"><span>{ viewers }</span>
    <img class="icon views"><span>{ views }</span>
    <img class="icon follows"><span>{ followers }</span>
  </div>

  <style scoped>
    .name, .game {
      color: #8D8B96;
      font-weight: bold;
    }

    .icon {
      height: 18px;
      width: 18px;
      margin-left: 10px;
      margin-right: 5px;
    }

    .joystick {
      content:url("/chromecast/img/icons/joystick.svg");
    }

    .viewers {
      content:url("/chromecast/img/icons/viewers.svg");
    }

    .views {
      content:url("/chromecast/img/icons/views.svg");
    }

    .follows {
      content:url("/chromecast/img/icons/follows.svg");
    }
  </style>

  <script>
    import TwitchAPI from 'api/twitch.js'

    const refreshInfoInterval = 30 // seconds

    this.channel = opts.channel

    this.fetchStreamInfos = () => {
      let self = this

      TwitchAPI.stream(self.channel)
        .then(data => {
          let stream = data.stream
          let channel = stream.channel

          self.name = channel.display_name
          self.game = channel.game

          self.viewers = stream.viewers.toLocaleString()
          self.views = channel.views.toLocaleString()
          self.followers = channel.followers.toLocaleString()

          self.update()
        })
    }

    this.on('mount', () => {
      this.fetchStreamInfos()

      setInterval(this.fetchStreamInfos, refreshInfoInterval * 1000)
    })

  </script>
</stream-info>
