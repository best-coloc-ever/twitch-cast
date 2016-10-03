<stream>

  <!-- Layout -->
  <notice></notice>
  <pause-indicator></pause-indicator>

  <div class="center">
    <video name="video" autoplay></video>
  </div>

  <clock show={ showStreamInfo }></clock>
  <stream-info show={ showStreamInfo }></stream-info>

  <!-- Style -->
  <style scoped>
    notice {
      position: absolute;
      text-align: center;
      color: white;
      width: 100%;
    }

    pause-indicator {
      position: absolute;
      top: 0;
      bottom: 0;
      left: 0;
      right: 0;
    }

    video {
      width: 100%;
      height: 100%;
      margin: auto auto;
      overflow: hidden !important;
    }

    .center {
      position: relative;
      top: 50%;
      transform: translateY(-50%);
    }

    .vjs_video_3-dimensions {
      width: 100% !important;
      height: 100% !important;
    }

    stream-info {
      position: absolute;
      bottom: 2%;
      right: 2%;
      text-align: right;
      color: white;
      z-index: -1;
    }

    clock {
      position: absolute;
      bottom: 2%;
      left: 2%;
      text-align: left;
      color: white;
    }
  </style>

  <!-- Logic -->
  <script>
    import PlayerEvent from 'player_events.js'

    this.showStreamInfo = true

    this.mediaElement = () => {
      return this.video
    }

    this.on('mount', () => {
      let self = this,
          // helper bindings
          player = this.parent.player,

          notice         = this.tags['notice'],
          streamInfo     = this.tags['stream-info'],
          pauseIndicator = this.tags['pauseIndicator']

      player.on(PlayerEvent.ChannelChanged, e => {
        streamInfo.setChannel(e.channel)
      })

      player.on(PlayerEvent.ChatToggled, e => {
        self.showStreamInfo = e.visible
        self.update()
      })

      player.on(PlayerEvent.AutoPaused, isPaused => {
        if (isPaused)
          notice.show('Buffering...')
        else
          notice.hide()

        pauseIndicator.setVisible(isPaused)
      })

      player.on(PlayerEvent.HostError, errorString => {
        notice.show(errorString)
      })

    })

  </script>

</stream>
