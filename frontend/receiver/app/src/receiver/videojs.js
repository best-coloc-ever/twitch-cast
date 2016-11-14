import ReceiverEvent from './events.js'
import StreamerAPI from 'api/streamer.js'
import { loadScript, loadLink } from 'utils/deferred_load.js'

const playerOptions = {
  nativeControlsForTouch: true,
  preload: true
}

const videojsCssUrl   = '//cdnjs.cloudflare.com/ajax/libs/video.js/5.11.3/video-js.min.css',
      videojsJsUrl    = '//cdnjs.cloudflare.com/ajax/libs/video.js/5.11.3/video.min.js',
      videojsHlsJsUrl = '//cdnjs.cloudflare.com/ajax/libs/videojs-contrib-hls/3.5.0/videojs-contrib-hls.min.js'

class VideojsReceiver {

  constructor(mediaElement) {
    this.mediaElement = mediaElement
    this.player = null

    // Add observer support
    riot.observable(this)

    riot.route('/*',   this.onChannelChanged.bind(this))
    riot.route('/*/*', this.onChannelAndQualityChanged.bind(this))

    // Loading videojs dynamically
    loadLink(videojsCssUrl)
    loadScript(videojsJsUrl)
      .then(() => loadScript(videojsHlsJsUrl))
      .then(this._initialize.bind(this))
  }

  onChannelChanged(channel) {
    StreamerAPI.stream(channel)
      .then(data => {
        data.playlists.sort((a, b) => b.bandwidth - a.bandwidth)
        let quality = data.playlists[0].name

        riot.route(`/${channel}/${quality}`, `${channel} (${quality})`, true)
      })
  }

  onChannelAndQualityChanged(channel, quality) {
    this.trigger(ReceiverEvent.ChannelChanged, { channel: channel })
    this._playStream(StreamerAPI.playlistUrl(channel, quality))
  }

  _initialize() {
    riot.route.start(true)
  }

  _playStream(playlistUrl) {
    this.player = videojs(
      this.mediaElement,
      playerOptions,
      () => { this.player.play() }
    )

    this.player.src({
      src: playlistUrl,
      type: 'application/vnd.apple.mpegurl'
    })

    this.mediaElement.setAttribute('controls', 'controls')
  }

}

module.exports = VideojsReceiver
