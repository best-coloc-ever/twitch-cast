import { loadScript } from 'utils/deferred_load.js'
import { ChromecastMessageType, chromecastCustomMessageBus } from 'chromecast/messages.js'
import { routeLinks } from 'routing/routes.js'

const chromecastSdkReceiverJsUrl = '//www.gstatic.com/cast/sdk/libs/receiver/2.0.0/cast_receiver.js'

export const ReceiverEvent = {
  Ready: 'receiver-ready'
}

export default class ChromecastReceiver {

  constructor(mediaElement) {
    this.mediaElement = mediaElement

    this.mediaManager = null
    this.customMessageBus = null

    riot.observable(this)

    loadScript(chromecastSdkReceiverJsUrl)
      .then(() => this._initialize())
  }

  _initialize() {
    // The mediaManager handles media messages
    let mediaManager = new cast.receiver.MediaManager(this.mediaElement)
    mediaManager.onLoad = this._onLoadEvent.bind(this)

    // The castManager allows communication with the sender application
    let castManager = cast.receiver.CastReceiverManager.getInstance()

    // Setting up a custom message bus to communicate with the sender application
    let customMessageBus = castManager.getCastMessageBus(chromecastCustomMessageBus)
    customMessageBus.onMessage = this._handleCustomMessages.bind(this)

    this.mediaManager = mediaManager
    this.customMessageBus = customMessageBus

    castManager.start()

    this.trigger(ReceiverEvent.Ready)
  }

  _onLoadEvent(event) {
    this.mediaManager.sendLoadComplete()

    let channel = event.data.customData.channel,
        quality = event.data.customData.quality

    let loadArgs = [channel]
    if (quality)
      loadArgs.push(quality)

    riot.route(routeLinks.load(...loadArgs))
  }

  _handleCustomMessages(event) {
    let message = JSON.parse(event.data)

    this.trigger(message.type, message.data)
  }

  sendCustomMessage(message) {
    this.customMessageBus.broadcast(JSON.stringify(message))
  }

}
