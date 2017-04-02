export const ChatClientEvent = {
  Joined: 'chat-client-joined',
  Closed: 'chat-client-closed',
  Error: 'chat-client-error',
  Messages: 'chat-client-messages',
}

const initialChatDelay = 3000, // milliseconds
      chatDisplayInterval = 250, // milliseconds
      initialReconnectTimeout = 2000 // milliseconds

export class ChatClient {

  constructor(channel) {
    riot.observable(this)

    this.channel = channel

    this.chatDelay = initialChatDelay
    this.reconnectTimeout = initialReconnectTimeout

    this.messageQueue = []
    this.processMessageQueueTimeoutID = null

    this.lastPause = null

    this.ws = this._initWebSocket()

    this.processMessageQueue = this._processMessageQueue.bind(this)
    this.processMessageQueue()
  }

  destruct() {
    this.ws.close()
    clearTimeout(this.processMessageQueueTimeoutID)
  }

  pause() {
    this.lastPause = Date.now()
  }

  unpause() {
    if (this.lastPause) {
      let additionalDelay = Date.now() - this.lastPause

      this.chatDelay += additionalDelay
      this.lastPause = null
    }
  }

  _chatUrl() {
    return `wss://${window.location.host}/chat/${this.channel}`
  }

  _initWebSocket() {
    let ws = new WebSocket(this._chatUrl())

    ws.onopen = this._onWSOpen.bind(this)
    ws.onclose = this._onWSClose.bind(this)
    ws.onError = this._onWSError.bind(this)
    ws.onmessage = this._onWSMessage.bind(this)

    return ws
  }

  _onWSOpen() {
    this.trigger(ChatClientEvent.Joined, this.channel)
  }

  _onWSClose(event) {
    this.trigger(ChatClientEvent.Closed, this.reconnectTimeout)

    if (event.code != 1000) {
      setTimeout(
        () => { this.ws = this._initWebSocket() },
        this.reconnectTimeout
      )
      this.reconnectTimeout *= 2
    }
  }

  _onWSError(error) {
    this.trigger(ChatClientEvent.Error, error.reason)
  }

  _onWSMessage(event) {
    let message = JSON.parse(event.data)
    message.stamp = new Date()
    this.messageQueue.push(message)
  }

  _processMessageQueue() {
    let now = new Date(),
        i = 0

    for (; i < this.messageQueue.length; ++i) {
      if (now - this.messageQueue[i].stamp < this.chatDelay)
        break
    }

    let poppedMessages = this.messageQueue.splice(0, i)

    this.processMessageQueueTimeoutID = setTimeout(
      this.processMessageQueue,
      chatDisplayInterval
    )
  }

}
