import { loadScript } from 'utils/deferred_load.js'
import ChromecastMessage, { chromecastCustomMessageBus } from './messages.js'
import StreamerAPI from 'api/streamer.js'

const chromecastSdkSenderJsUrl = '//www.gstatic.com/cv/js/sender/v1/cast_sender.js?loadCastFramework=1'

export const SenderEvent = {
  Initialized: 'sender-initialized',
  CastStateChanged: 'sender-cast-state-changed',
  SessionStateChanged: 'sender-session-state-changed',
  ChannelSent: 'sender-channel-sent',
  ChannelQueued: 'sender-channel-queued',
  CastError: 'sender-cast-error'
}

export class ChromecastSender {

  constructor() {
    this.castContext = null
    this.queue = null

    riot.observable(this)

    this.on(SenderEvent.CastStateChanged, state => {
      if (this.queue && state == cast.framework.CastState.CONNECTED) {
        this._playImpl(...this.queue)
        this.queue = null
      }
    })

    window.__onGCastApiAvailable = isAvailable => {
      if (isAvailable)
        this.initialize()

      this.trigger(SenderEvent.Initialized)
    }

    loadScript(chromecastSdkSenderJsUrl)
  }

  initialize() {
    const castContextOptions = {
      receiverApplicationId: CHROMECAST_APP_ID,
      autoJoinPolicy:        chrome.cast.AutoJoinPolicy.ORIGIN_SCOPED,
      resumeSavedSession:    true
    }

    this.on(SenderEvent.SessionStateChanged, state => this.onSessionStateChanged(state))

    let castContext = cast.framework.CastContext.getInstance()
    castContext.setOptions(castContextOptions)

    castContext.addEventListener(
      cast.framework.CastContextEventType.CAST_STATE_CHANGED,
      event => this.trigger(SenderEvent.CastStateChanged, event.castState)
    )
    castContext.addEventListener(
      cast.framework.CastContextEventType.SESSION_STATE_CHANGED,
      event => this.trigger(SenderEvent.SessionStateChanged, event)
    )

    this.castContext = castContext
  }

  connect() {
    this.castContext.requestSession()
      .then(
        mbError => undefined,
        error   => undefined
      )
  }

  onSessionStateChanged(event) {
    if (event.session) {
      event.session.addMessageListener(chromecastCustomMessageBus, (_, data) => {
        let message = JSON.parse(data)

        this.trigger(message.type, message.data)
      })
    }
  }

  connected() {
    return this.castContext.getCastState() == cast.framework.CastState.CONNECTED
  }

  disconnect() {
    this.castContext.endCurrentSession(true)
  }

  sendCustomMessage(message) {
    let session = this.castContext.getCurrentSession()

    if (!session)
      return new Promise((_, onError) => { onError() })

    return session.sendMessage(
      chromecastCustomMessageBus,
      message
    )
  }

  deviceName() {
    let session = this.castContext.getCurrentSession()

    if (session)
      return session.getSessionObj().receiver.friendlyName

    return 'Unknown device'
  }

  play(channel, quality = null) {
    if (!this.castContext)
      this.playLocally(channel, quality)

    this.queue = null

    let castState = this.castContext.getCastState()

    if (castState == cast.framework.CastState.CONNECTED)
      this._playImpl(channel, quality)
    else if (castState == cast.framework.CastState.CONNECTING) {
      this.queue = [channel, quality]
      this.trigger(SenderEvent.ChannelQueued, channel)
    }
    else
      this.playLocally(channel, quality)
  }

  _playImpl(channel, quality) {
    let mediaInfo = new chrome.cast.media.MediaInfo(),
        request = new chrome.cast.media.LoadRequest(mediaInfo)

    request.customData = {
      channel: channel,
      quality: quality
    }

    this.castContext.getCurrentSession().loadMedia(request)
    this.trigger(SenderEvent.ChannelSent, channel)
  }

  playLocally(channel, quality = null) {
    let url = StreamerAPI.receiverUrl(channel)
    window.location = url
  }

  onCastError(error) {
    this.trigger(SenderEvent.CastError, error)
  }

}
