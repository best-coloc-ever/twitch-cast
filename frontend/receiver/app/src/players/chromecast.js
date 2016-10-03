import PlayerEvent from 'player_events.js'

const customMessageBusName       = 'urn:x-cast:twitch.cast.message',
      // Host settings
      autoPauseDuration          = 3,
      autoResumeDuration         = 3,
      autoResumeNumberOfSegments = 1,
      segmentRequestRetryLimit   = 2

cast.player.api.setLoggerLevel(cast.player.api.LoggerLevel.NONE)

class ChromecastPlayer {

  constructor(mediaElement) {
    this.mediaElement = mediaElement
    this.player = null

    // Add observer support
    riot.observable(this)

    // The mediaManager handles media messages
    let mediaManager = new cast.receiver.MediaManager(mediaElement)
    mediaManager.onLoad = this._onLoadEvent

    // The castManager allows communication with the sender application
    let castManager = cast.receiver.CastReceiverManager.getInstance()

    // Setting up a custom message bus to communicate with the sender application
    let customMessageBus = castManager.getCastMessageBus(customMessageBusName)
    customMessageBus.onMessage = this._handleCustomMessages

    castManager.start()
  }

  _onLoadEvent(event) {
    this._unloadPlayer()

    if (event.data['media'] && event.data['media']['contentId']) {
      let url = event.data['media']['contentId']
      let host = this._makeHost(url)
      let protocol = cast.player.api.CreateHlsStreamingProtocol(host)

      this.player = new cast.player.api.Player(host)
      this.player.load(protocol, 0)
    }
  }

  _handleCustomMessages(event) {
    let message = JSON.parse(event.data)

    this.trigger(message.type, message)
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

    host.onError = this._onHostError
    host.onAutoPause = this._onHostAutoPause

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

    this.trigger(PlayerEvent.HostError, errorString)
  }

  _onHostAutoPause(isPaused) {
    this.trigger(PlayerEvent.AutoPaused, isPaused)
  }

}

module.exports = ChromecastPlayer
