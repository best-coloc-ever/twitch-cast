<stream-view>

  <!-- layout -->
  <div id="main" class={ 'reverse-flex': chatLeft }>
    <div id="player">
      <notice player={ player }></notice>
      <pause-indicator player={ player }></pause-indicator>

      <div class="center" name="container">
      </div>

      <clock show={ !fullScreen }></clock>
      <stream-info show={ !fullScreen } channel={ channel }></stream-info>
    </div>

    <chat channel={ channel } player={ player } show={ !fullScreen }></chat>
  </div>


  <!-- style -->
  <style scoped>
    #main {
      height: 100%;
      display: flex;
    }

    .reverse-flex {
      flex-direction: row-reverse;
    }

    #player {
      flex: 1 1 auto;
      height: 100%;
      position: relative;
    }

    chat {
      flex: 0 1 300px;
      min-width: 300px;
    }

    .center {
      position: relative;
      top: 50%;
      transform: translateY(-50%);
    }

    video {
      width: 100%;
      height: 100%;
      margin: auto auto;
      overflow: hidden !important;
    }

    .vjs-controls-disabled {
      width: unset;
      height: unset;
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

    notice {
      position: absolute;
      text-align: center;
      color: white;
      width: 100%;
    }
  </style>


  <!-- logic -->
  <script>
    import { isChromecastDevice } from 'utils/platform.js'

    import { ChromecastMessageType, ChatPositions } from 'chromecast/messages.js'

    import StreamerAPI from 'api/streamer.js'

    let [channel, quality] = opts.routeOpts
    this.player = opts.player

    this.channel = channel
    this.quality = quality
    this.fullScreen = false
    this.chatLeft = false

    this.on('mount', () => {
      opts.receiver.on(ChromecastMessageType.ToggleFullscreen, data => {
        this.fullScreen = data.enabled
        this.update()
      })

      opts.receiver.on(ChromecastMessageType.ChatPosition, data => {
        this.chatLeft = (data.position == ChatPositions.Left)
        console.log(data.position, ChatPositions.Left)
        this.update()
      })

      this.container.appendChild(opts.video)

      let playlistUrl = StreamerAPI.playlistUrl(channel, quality)
      this.player.play(playlistUrl)
    })

    this.on('before-unmount', () => {
      this.player.stop()
    })
  </script>

</stream-view>
