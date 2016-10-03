<app>

  <!-- Layout -->
  <div class="box">

    <stream class="flex"></stream>

    <chat show={ chatVisible }></chat>

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
    import ChromecastPlayer from 'players/chromecast.js'
    import VideojsPlayer from 'players/videojs.js'
    import PlayerEvent from 'player_events.js'

    this.chatVisible = true
    this.player = null

    this.on('mount', () => {
      let self = this,
          isChromecastDevice = (navigator.userAgent.indexOf('CrKey') != -1),
          playerClass = (isChromecastDevice ? ChromecastPlayer : VideojsPlayer),
          mediaElement = this.tags.stream.mediaElement()

      let player = new playerClass(mediaElement)

      player.on(PlayerEvent.ChatToggled, e => {
        self.chatVisible = e.visible
        self.update()
      })

      // TODO: move the chat in the dom (bypass riot or not ?)
      player.on(PlayerEvent.ChatPositionChanged, e => {

      })

      this.player = player
    })

  </script>

</app>
