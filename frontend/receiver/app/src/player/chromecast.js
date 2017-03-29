import { PlayerEvent } from './events.js'
import { loadScript } from 'utils/deferred_load.js'

const chromecastSdkMediaplayerJsUrl = '//www.gstatic.com/cast/sdk/libs/mediaplayer/1.0.0/media_player.js'

      // Host settings
const autoPauseDuration          = 2,
      autoResumeDuration         = 2,
      autoResumeNumberOfSegments = 1,
      segmentRequestRetryLimit   = 3

export default class ChromecastPlayer {

  constructor(mediaElement) {
    this.mediaElement = mediaElement
    this._player = null

    riot.observable(this)

    loadScript(chromecastSdkMediaplayerJsUrl)
      .then(() => this._initialize())
  }

  play(playlistUrl) {
    this.stop()

    let host = this._makeHost(playlistUrl)
    let protocol = cast.player.api.CreateHlsStreamingProtocol(host)

    this._player = new cast.player.api.Player(host)
    this._player.load(protocol, Infinity)
  }

  stop() {
    if (this._player) {
      this._player.unload()
      this._player = null
    }
  }

  delay() {
    return this._player.getMaxBufferDuration(0) - this._player.getBufferDuration(0)
  }

  _initialize() {
    cast.player.api.setLoggerLevel(cast.player.api.LoggerLevel.NONE)

    this.trigger(PlayerEvent.Ready)
  }

  _makeHost(url) {
    let host = new cast.player.api.Host({
      mediaElement: this.mediaElement,
      url: url
    })

    host.autoPauseDuration = autoPauseDuration
    host.autoResumeDuration = autoResumeDuration
    host.autoResumeNumberOfSegments = autoResumeNumberOfSegments
    host.segmentRequestRetryLimit = segmentRequestRetryLimit
    // Workaround (hopefully temporary)
    host.updateManifestRequestInfo = info => {
      info.timeoutInterval = 500
    }

    host.onError = this._onHostError.bind(this)
    host.onAutoPause = this._onHostAutoPause.bind(this)
    host.onMediaDownloadEnded = this._onMediaDownloadEnded.bind(this)

    return host
  }

  _onHostError(errorCode, request) {
    let errorString = `Fatal Error: ${errorCode}`
    if (request)
      errorString += ` (${request.errorCode}, ${request.status})`

    this.trigger(PlayerEvent.HostError, errorString)
  }

  _onHostAutoPause(isPaused) {
    this.trigger(PlayerEvent.AutoPaused, isPaused)
  }

  _onMediaDownloadEnded() {
    this.trigger(PlayerEvent.MediaEnd)
  }
}
