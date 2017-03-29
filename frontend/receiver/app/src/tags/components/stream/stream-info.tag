<stream-info>

  <span class="name">{ name }</span>
  <span class="quality"> ({ quality }) </span> <img class="icon joystick">
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

    .quality {
      color: #8D8B96;
      font-size: 75%;
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

    let fetchTimer = null

    this.channel = opts.channel
    this.quality = opts.quality

    this.fetchStreamInfos = () => {
      TwitchAPI.stream(this.channel)
        .then(data => {
          let stream = data.stream
          let channel = stream.channel

          this.name = channel.display_name
          this.game = channel.game

          this.viewers = stream.viewers.toLocaleString()
          this.views = channel.views.toLocaleString()
          this.followers = channel.followers.toLocaleString()

          this.update()
        })
    }

    this.on('mount', () => {
      this.fetchStreamInfos()

      fetchTimer = setInterval(this.fetchStreamInfos, refreshInfoInterval * 1000)
    })

    this.on('unmount', () => {
      clearInterval(fetchTimer)
    })

  </script>
</stream-info>
