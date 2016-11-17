<stream-view>

  <!-- layout -->
  <div id="main">
    <div id="player">
      <notice></notice>
      <pause-indicator></pause-indicator>

      <div class="center" name="container">
      </div>

      <clock show={ showStreamInfo }></clock>
      <stream-info show={ showStreamInfo } channel={ channel }></stream-info>
    </div>

    <chat channel={ channel } player={ player }></chat>
  </div>


  <!-- style -->
  <style scoped>
    #main {
      height: 100%;
      display: flex;
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
  </style>


  <!-- logic -->
  <script>
    import { isChromecastDevice } from 'utils/platform.js'

    import StreamerAPI from 'api/streamer.js'

    let [channel, quality] = opts.routeOpts
    this.player = opts.player

    this.channel = channel
    this.quality = quality
    this.showStreamInfo = true

    this.on('mount', () => {
      this.container.appendChild(opts.video)
      let playlistUrl = StreamerAPI.playlistUrl(channel, quality)
      this.player.play(playlistUrl)
    })

    this.on('before-unmount', () => {
      this.player.stop()
    })
  </script>

</stream-view>
