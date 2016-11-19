import { loadScript } from 'utils/deferred_load.js'
import { ChromecastMessageType, chromecastCustomMessageBus } from 'chromecast/messages.js'

const chromecastSdkReceiverJsUrl = '//www.gstatic.com/cast/sdk/libs/receiver/2.0.0/cast_receiver.js'

export const ReceiverEvent = {
  Ready: 'receiver-ready'
}

export default class ChromecastReceiver {

  constructor() {
    this.customMessageBus = null

    riot.observable(this)

    loadScript(chromecastSdkReceiverJsUrl)
      .then(() => this._initialize())

    this.on(ChromecastMessageType.Watch, data => {
      if (data.quality)
        riot.route(`/${data.channel}/${data.quality}`)
      else
        riot.route(`/${data.channel}`)
    })
  }

  _initialize() {
    // The castManager allows communication with the sender application
    let castManager = cast.receiver.CastReceiverManager.getInstance()

    // Setting up a custom message bus to communicate with the sender application
    let customMessageBus = castManager.getCastMessageBus(chromecastCustomMessageBus)
    customMessageBus.onMessage = this._handleCustomMessages.bind(this)
    this.customMessageBus = customMessageBus

    castManager.start()

    this.trigger(ReceiverEvent.Ready)
  }

  _handleCustomMessages(event) {
    let message = JSON.parse(event.data)

    this.trigger(message.type, message.data)
  }

  sendCustomMessage(message) {
    this.customMessageBus.broadcast(JSON.stringify(message))
  }

}
