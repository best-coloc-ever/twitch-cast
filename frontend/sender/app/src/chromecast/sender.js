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

export default class ChromecastSender {

  constructor() {
    this.castContext = null
    this.playOnReady = null

    riot.observable(this)

    this.on(SenderEvent.CastStateChanged, state => {
      if (this.playOnReady && cast.framework.CastState.CONNECTED)
        this.sendCustomMessage(ChromecastMessage.watch(this.playOnReady))
          .then(
            mbError => { if (mbError) this.onCastError(mbError) },
            error   => this.onCastError(error)
          )
    })

    window.__onGCastApiAvailable = isAvailable => {
      if (isAvailable)
        this.initialize()
      else
        console.log('unavailable')
    }

    loadScript(chromecastSdkSenderJsUrl)
  }

  initialize() {
    const castContextOptions = {
      receiverApplicationId: CHROMECAST_APP_ID,
      autoJoinPolicy:        chrome.cast.AutoJoinPolicy.ORIGIN_SCOPED,
      resumeSavedSession:    true
    }

    this.on(SenderEvent.SessionStateChanged, console.log.bind(console))

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

    this.trigger(SenderEvent.Initialized)
  }

  connect() {
    this.castContext.requestSession()
      .then(
        mbError => undefined,
        error   => undefined
      )
  }

  disconnect() {
    this.castContext.endCurrentSession(true)
  }

  sendCustomMessage(message) {
    let session = this.castContext.getCurrentSession()

    if (!session)
      return new Promise((..._) => { })

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

  play(channel) {
    this.playOnReady = null

    let castState = this.castContext.getCastState()

    if (castState == cast.framework.CastState.CONNECTED) {
      this.sendCustomMessage(ChromecastMessage.watch(channel))
        .then(
          mbError => {
            if (!mbError)
              this.trigger(SenderEvent.ChannelSent, channel)
            else
              this.onCastError(mbError)
          },
          error   => this.onCastError(error)
        )
    }
    else if (castState == cast.framework.CastState.CONNECTING) {
      this.playOnReady = channel
      this.trigger(SenderEvent.ChannelQueued, channel)
    }
    else
      this.playLocally(channel)
  }

  playLocally(channel) {
    let url = StreamerAPI.receiverUrl(channel)
    window.location = url
  }

  onCastError(error) {
    this.trigger(SenderEvent.CastError, error)
  }

}
