<chat>

  <!-- Layout -->
  <ul id="chat" name="chat">
  </ul>

  <!-- Style -->
  <style scoped>
    :scope {
      background: #19191f;
      overflow-y: hidden;
      overflow-x: hidden;
      word-wrap: break-word;
    }

    ul {
      padding-left: 10px
    }

    li {
      color: white
    }
  </style>


  <!-- Logic -->
  <script>
    import ChatAssetStore from 'chat/asset_store.js'
    import { buildChatLine } from 'chat/message.js'
    import { ChromecastMessageType } from 'chromecast/messages.js'

    const maxChatMessageCount = 50
    const chatDisplayInterval = 0.3 // seconds

    let channel = opts.channel
    let player = opts.player

    let chatUrl = `wss://${window.location.host}/chat/${channel}`
    let chatDelay = 2
    let reconnectTimeout = 2

    let store = new ChatAssetStore
    store.loadChannelBadges(channel)

    let messageQueue = []
    let messages = []

    this.notify = text => {
      this.addMessage({ sender: 'SYSTEM', content: text })
      this.update()
    }

    this.onmessage = (e) => {
      let message = JSON.parse(e.data)
      message.stamp = new Date().getTime()
      messageQueue.push(message)
    }

    this.pause = () => {
      this.notify('Pausing chat')
      ws.onmessage = null
    }

    this.resume = () => {
      this.notify('Resuming chat')
      ws.onmessage = this.onmessage
    }

    this.connectToChat = () => {
      let ws = new WebSocket(chatUrl)

      ws.onopen = () => {
        this.notify(`Successfully joined ${channel}'s chatroom`)
      }

      ws.onclose = e => {
        this.notify(`Connection closed: reconnecting in ${reconnectTimeout} seconds`)

        if (e.code != 1000) {
          setTimeout(this.connectToChat, reconnectTimeout * 1000)
          reconnectTimeout *= 2
        }
      }

      ws.onerror = function(e) {
        this.notify('Error: ' + e.reason)
      }

      ws.onmessage = this.onmessage
    }

    this.addMessage = message => {
      messages.push(message)
      // JQuery
      var chatLine = buildChatLine(message, store)
      $(this.chat).append(chatLine)

      var toSlice = Math.max(0, messages.length - maxChatMessageCount)
      messages = messages.slice(toSlice)

      $(this.chat).find('li:lt(' + toSlice + ')').remove()
    }

    this.processMessageQueue = () => {
      var i = 0
      var now = new Date().getTime()

      for (; i < messageQueue.length; ++i) {
        var message = messageQueue[i]

        if (now - message.stamp < chatDelay * 1000)
          break

        this.addMessage(message)
      }

      messageQueue.splice(0, i)

      this.update()
      this.root.scrollTop = this.root.scrollHeight

      setTimeout(this.processMessageQueue, chatDisplayInterval * 1000)
    }

    this.updateChatDelay = () => {
      this.chatDelay = player.delay()
    }

    this.on('mount', () => {
      this.connectToChat()
      this.processMessageQueue()

      // setInterval(this.updateChatDelay, 3000)
    })

  </script>

</chat>
