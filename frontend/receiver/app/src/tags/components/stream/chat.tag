<chat>

  <!-- Layout -->
  <ul ref="staticChat" show={ opts.twoPart } class="static-chat">
  </ul>
  <ul ref="scrollingChat">
  </ul>

  <!-- Style -->
  <style scoped>
    :scope {
      background: #19191f;
      overflow-y: hidden;
      overflow-x: hidden;
      word-wrap: break-word;
    }

    .static-chat {
      height: 50%;
      border-bottom: 1px solid white;
      overflow-y: hidden;
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
    import { ChatClient, ChatClientEvent } from 'chat/client.js'
    import ChatAssetStore from 'chat/asset_store.js'
    import { buildChatLine } from 'chat/message.js'

    const maxChatMessageCount = 50

    let channel = opts.channel

    let store = new ChatAssetStore(channel)
    let client = new ChatClient(channel)

    let scrollingMessages = []

    this.addMessagesLogic = () => {
      return opts.twoPart ?
        this.addMessagesTwoPart :
        this.addMessagesScrolling
    }

    this.addMessages = this.addMessagesLogic()

    this.notify = text => {
      this.addMessages({ sender: 'SYSTEM', content: text })
    }

    this.pause = () => {
      client.pause()
    }

    this.unpause = () => {
      client.unpause()
    }

    this.addMessagesScrolling = (...messages) => {
      messages.forEach((message) => {
        let chatLine = buildChatLine(message, store)
        this.scrollingChat.append(chatLine)
      })

      scrollingMessages.concat(messages)
      let toSlice = Math.max(0, scrollingMessages.length - maxChatMessageCount)
      scrollingMessages = scrollingMessages.slice(toSlice)

      this.scrollingChat.find('li:lt(' + toSlice + ')').remove()

      this.root.scrollTop = this.root.scrollHeight
    }

    this.addMessagesTwoPart = (...messages) => {
      let heightLimit = this.root.clientHeight / 2

      messages.forEach((message) => {
        let chatLine = buildChatLine(message, store)
        this.scrollingChat.append(chatLine)

        if (this.scrollingChat.height() + chatLine.height() >= heightLimit) {
          chatLine.detach()
          this.staticChat.empty()
          this.scrollingChat.contents().appendTo(this.staticChat)
          this.scrollingChat.empty()
          this.scrollingChat.append(chatLine)
        }
      })
    }

    this.on('mount', () => {
      client.on(ChatClientEvent.Joined, (channel) => {
        this.notify(`Successfully joined ${channel}'s chatroom`)
      })

      client.on(ChatClientEvent.Closed, (reconnectTimeout) => {
        this.notify(`Connection closed: reconnecting in ${reconnectTimeout / 1000} seconds`)
      })

      client.on(ChatClientEvent.Error, (reason) => {
        this.notify('Error: ' + reason)
      })

      client.on(ChatClientEvent.Messages, (messages) => {
        this.addMessages(...messages)
      })

      this.scrollingChat = $(this.refs.scrollingChat)
      this.staticChat = $(this.refs.staticChat)
    })

    this.on('unmount', () => {
      client.destroy()
    })

    this.on('update', () => {
      console.log('UPDATED')
      this.addMessages = this.addMessagesLogic()
      scrollingMessages = []
    })

  </script>

</chat>
