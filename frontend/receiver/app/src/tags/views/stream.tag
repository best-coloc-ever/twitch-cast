<stream-view>

  <!-- layout -->
  <div id="main" class={ 'reverse-flex': chatLeft }>
    <div id="player">
      <notice></notice>
      <pause-indicator if={ isPaused }></pause-indicator>

      <div class="center" ref="container">
      </div>

      <clock show={ !fullScreen }></clock>
      <stream-info show={ !fullScreen } channel={ channel } quality={ quality }></stream-info>
    </div>

    <chat ref="chat" channel={ channel } show={ !fullScreen } two-part={ twoPartChat }></chat>
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
    import { ChromecastMessageType, ChatPositions, ChatFlavors } from 'chromecast/messages.js'
    import { PlayerEvent } from 'player/events.js'
    import { Mixins } from 'context/mixins.js'
    import StreamerAPI from 'api/streamer.js'

    let [channel, quality] = opts.path

    this.channel = channel
    this.quality = quality
    this.fullScreen = false
    this.chatLeft = false
    this.isPaused = false
    this.twoPartChat = false

    // Receiver events
    this.onFullscreenToggled = data => this.update({ fullScreen: data.enabled })
    this.onChatPositionChanged = data => this.update({ chatLeft: (data.position == ChatPositions.Left) })
    this.onChatSizeChanged = data => {
      let sizeStr = `${data.size}px`

      this.refs.chat.root.style['min-width'] = sizeStr
      this.refs.chat.root.style['flex'] = `0 1 ${sizeStr}`

      this.update()
    }
    this.onChatFlavorChanged = data => this.update({ twoPartChat: (data.flavor == ChatFlavors.TwoPart) })

    let receiverEvents = [
      [ChromecastMessageType.ToggleFullscreen, this.onFullscreenToggled],
      [ChromecastMessageType.ChatPosition, this.onChatPositionChanged],
      [ChromecastMessageType.ChatSize, this.onChatSizeChanged],
      [ChromecastMessageType.ChatFlavor, this.onChatFlavorChanged],
    ]

    // Player events
    this.onMediaEnd = () => this.tags.notice.show('Stream ended')
    this.onHostError = error => this.tags.notice.show(error)
    this.onPlayerPaused = isPaused => {
      this.update({ isPaused: isPaused })

      if (isPaused) {
        this.tags.notice.show('Buffering...')
        this.tags.chat.pause()
      }
      else {
        this.tags.notice.hide()
        this.tags.chat.unpause()
      }
    }

    let playerEvents = [
      [PlayerEvent.MediaEnd, this.onMediaEnd],
      [PlayerEvent.HostError, this.onHostError],
      [PlayerEvent.AutoPaused, this.onPlayerPaused],
    ]

    this.on('mount', () => {
      this.mixin(Mixins.Receiver)
      this.mixin(Mixins.Player)

      receiverEvents.forEach(action => this.receiver.on(...action))
      playerEvents.forEach(action => this.player.on(...action))

      this.refs.container.appendChild(this.player.mediaElement)

      let playlistUrl = StreamerAPI.playlistUrl(channel, quality)
      this.player.play(playlistUrl)
    })

    this.on('unmount', () => {
      receiverEvents.forEach(action => this.receiver.off(...action))
      playerEvents.forEach(action => this.player.off(...action))

      this.player.stop()
    })
  </script>

</stream-view>
