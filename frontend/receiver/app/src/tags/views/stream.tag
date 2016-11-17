<stream-view>

  <!-- layout -->
  <div id="main">
    <div id="player">
      <notice></notice>
      <pause-indicator></pause-indicator>

      <div class="center">
        <video name="video" autoplay></video>
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

    .vjs_video_1-dimensions {
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


  <!-- logic -->
  <script>
    import { isChromecastDevice } from 'utils/platform.js'

    import { PlayerEvent } from 'player/events.js'
    import ChromecastPlayer from 'player/chromecast.js'
    import VideojsPlayer from 'player/videojs.js'
    import StreamerAPI from 'api/streamer.js'

    let [channel, quality] = opts.routeOpts
    let playerClass = (
      isChromecastDevice ?
      ChromecastPlayer :
      VideojsPlayer
    )

    this.channel = channel
    this.quality = quality
    this.showStreamInfo = true
    this.player = new playerClass(this.video)

    this.on('mount', () => {
      let playlistUrl = StreamerAPI.playlistUrl(channel, quality)

      this.player.on(PlayerEvent.Ready, () => {
        this.player.play(playlistUrl)
      })
    })

    this.on('unmount', () => {
      this.player.delete()
    })
  </script>

</stream-view>
