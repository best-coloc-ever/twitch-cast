import ReceiverEvent from './events.js'
import StreamerAPI from 'api/streamer.js'
import { loadScript } from 'utils/deferred_load.js'
import { ChromecastMessageType } from 'chromecast/messages.js'

const customMessageBusName       = 'urn:x-cast:twitch.cast.message',
      // Host settings
      autoPauseDuration          = 3,
      autoResumeDuration         = 3,
      autoResumeNumberOfSegments = 1,
      segmentRequestRetryLimit   = 5

const chromecastSdkReceiverJsUrl    = '//www.gstatic.com/cast/sdk/libs/receiver/2.0.0/cast_receiver.js',
      chromecastSdkMediaplayerJsUrl = '//www.gstatic.com/cast/sdk/libs/mediaplayer/1.0.0/media_player.js'

class ChromecastReceiver {

  constructor(mediaElement) {
    this.mediaElement = mediaElement
    this.player = null

    // Making sure autoplay is set
    this.mediaElement.autoplay = true

    // Add observer support
    riot.observable(this)

    this.on(ChromecastMessageType.Watch, data => this.playChannel(data.channel))

    // Loading the chromecast sdk dynamically
    loadScript(chromecastSdkReceiverJsUrl)
      .then(() => loadScript(chromecastSdkMediaplayerJsUrl))
      .then(this._initialize.bind(this))
  }

  _initialize() {
    cast.player.api.setLoggerLevel(cast.player.api.LoggerLevel.NONE)

    // The castManager allows communication with the sender application
    let castManager = cast.receiver.CastReceiverManager.getInstance()

    // Setting up a custom message bus to communicate with the sender application
    let customMessageBus = castManager.getCastMessageBus(customMessageBusName)
    customMessageBus.onMessage = this._handleCustomMessages.bind(this)

    castManager.start()
  }

  _handleCustomMessages(event) {
    let message = JSON.parse(event.data)

    this.trigger(message.type, message.data)
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
      info.timeoutInterval = 2000
    }

    host.onError = this._onHostError.bind(this)
    host.onAutoPause = this._onHostAutoPause.bind(this)

    return host
  }

  _unloadPlayer() {
    if (this.player !== null) {
      this.player.unload()
      this.player = null
    }
  }

  _onHostError(errorCode, request) {
    this._unloadPlayer()

    let errorString = `Fatal Error: ${errorCode}`
    if (request)
      errorString += ` (${request.errorCode}, ${request.status})`

    this.trigger(ReceiverEvent.HostError, errorString)
  }

  _onHostAutoPause(isPaused) {
    this.trigger(ReceiverEvent.AutoPaused, isPaused)
  }

  playChannel(channel) {
    this.trigger(ReceiverEvent.ChannelChanged, { channel: channel })

    StreamerAPI.stream(channel)
      .then(data => {
        data.playlists.sort((a, b) => b.bandwidth - a.bandwidth)
        let quality = data.playlists[0].name

        this.playChannelAndQuality(channel, quality)
      })
  }

  playChannelAndQuality(channel, quality) {
    this._unloadPlayer()

    let url = StreamerAPI.playlistUrl(channel, quality)
    let host = this._makeHost(url)
    let protocol = cast.player.api.CreateHlsStreamingProtocol(host)

    this.player = new cast.player.api.Player(host)
    this.player.load(protocol, Infinity)
  }

}

module.exports = ChromecastReceiver
