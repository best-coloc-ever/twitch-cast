import ReceiverEvent from './events.js'
import StreamerAPI from 'api/streamer.js'

const playerOptions = {
  nativeControlsForTouch: true,
  preload: true
}

class VideojsReceiver {

  constructor(mediaElement) {
    this.mediaElement = mediaElement
    this.player = null

    // Add observer support
    riot.observable(this)

    // Fetching stream id from query parameters
    var streamId = riot.route.query().id

    if (streamId)
      this._fetchStream(streamId)
  }

  _fetchStream(streamId) {
    StreamerAPI.stream(streamId)
      .then(data => {
        this.trigger(ReceiverEvent.ChannelChanged, { channel: data.channel })
        this._playStream(data.proxy.indexUrl)
      })
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
