import { loadScript } from 'utils/deferred_load.js'
import ChromecastMessage, { chromecastCustomMessageBus } from './messages.js'

const chromecastSdkSenderJsUrl = '//www.gstatic.com/cv/js/sender/v1/cast_sender.js?loadCastFramework=1'

export const SenderEvent = {
  Initialized: 'sender-initialized',
  CastStateChanged: 'sender-cast-state-changed',
  SessionStateChanged: 'sender-session-state-changed',
  ChannelSent: 'sender-channel-sent'
}

export default class ChromecastSender {

  constructor() {
    this.castContext = null

    riot.observable(this)

    riot.route('/watch/*', channel => {
      this.sendCustomMessage(ChromecastMessage.watch(channel))
        .then(mbError => {
          if (!mbError)
            this.trigger(SenderEvent.ChannelSent, channel)
        })
    })

    window.__onGCastApiAvailable = isAvailable => {
      if (isAvailable)
        this.initialize()
    }

    loadScript(chromecastSdkSenderJsUrl)
  }

  initialize() {
    const castContextOptions = {
      receiverApplicationId: CHROMECAST_APP_ID,
      autoJoinPolicy:        chrome.cast.AutoJoinPolicy.ORIGIN_SCOPED,
      resumeSavedSession:    true
    }

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

  connected() {
    let session = this.castContext.getCurrentSession()

    if (session)
      return session.getSessionObj().status == chrome.cast.SessionStatus.CONNECTED

    return false
  }

}
