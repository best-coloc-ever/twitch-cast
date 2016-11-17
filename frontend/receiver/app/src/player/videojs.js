import { loadScript, loadLink } from 'utils/deferred_load.js'
import { PlayerEvent } from './events.js'

const videojsCssUrl   = '//cdnjs.cloudflare.com/ajax/libs/video.js/5.11.3/video-js.min.css',
      videojsJsUrl    = '//cdnjs.cloudflare.com/ajax/libs/video.js/5.11.3/video.min.js',
      videojsHlsJsUrl = '//cdnjs.cloudflare.com/ajax/libs/videojs-contrib-hls/3.5.0/videojs-contrib-hls.min.js'

const playerOptions = {
  nativeControlsForTouch: true,
  preload: true
}

export default class VideojsPlayer {

  constructor(mediaElement) {
    this.mediaElement = mediaElement
    this._player = null

    riot.observable(this)

    // Loading videojs dynamically
    loadLink(videojsCssUrl)
    loadScript(videojsJsUrl)
      .then(() => loadScript(videojsHlsJsUrl))
      .then(() => this._initialize())
  }

  play(playlistUrl) {


    this._player.src({
      src: playlistUrl,
      type: 'application/vnd.apple.mpegurl'
    })

    this.mediaElement.setAttribute('controls', 'controls')
  }

  stop() {

  }

  delay() {
    let currentTime = this._player.currentTime()
    let bufferEnd = this._player.bufferedEnd()

    return (bufferEnd - currentTime)
  }

  _initialize() {
    this._player = videojs(
      this.mediaElement,
      playerOptions,
      () => { this._player.play() }
    )

    this.trigger(PlayerEvent.Ready)
  }

}
