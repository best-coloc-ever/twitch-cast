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

    this.notify = text => {
      this.addMessages([{ sender: 'SYSTEM', content: text }])
    }

    this.pause = () => {
      this.client.pause()
    }

    this.unpause = () => {
      this.client.unpause()
    }

    this.addMessagesLogic = () => {
      return opts.twoPart ?
        this.addMessagesTwoPart :
        this.addMessagesScrolling
    }

    this.addMessagesScrolling = (messages) => {
      messages.forEach((message) => {
        let chatLine = buildChatLine(message, this.store)
        this.scrollingChat.append(chatLine)
      })

      this.scrollingMessageCount += messages.length
      let toSlice = Math.max(0, this.scrollingMessageCount - maxChatMessageCount)
      this.scrollingMessageCount -= toSlice

      this.scrollingChat.find('li:lt(' + toSlice + ')').remove()

      this.root.scrollTop = this.root.scrollHeight
    }

    this.addMessagesTwoPart = (messages) => {
      let heightLimit = this.root.clientHeight / 2

      messages.forEach((message) => {
        let chatLine = buildChatLine(message, this.store)
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
      this.channel = opts.channel

      this.scrollingChat = $(this.refs.scrollingChat)
      this.staticChat = $(this.refs.staticChat)

      this.addMessages = this.addMessagesLogic()
      this.scrollingMessageCount = 0

      this.store = new ChatAssetStore(this.channel)
      this.client = new ChatClient(this.channel, (ms) => this.addMessages(ms))

      this.client.on(ChatClientEvent.Joined, (channel) => {
        this.notify(`Successfully joined ${channel}'s chatroom`)
      })

      this.client.on(ChatClientEvent.Closed, (reconnectTimeout) => {
        this.notify(`Connection closed: reconnecting in ${reconnectTimeout / 1000} seconds`)
      })

      this.client.on(ChatClientEvent.Error, (reason) => {
        this.notify('Error: ' + reason)
      })
    })

    this.on('unmount', () => {
      this.client.destroy()
    })

    this.on('update', () => {
      this.addMessages = this.addMessagesLogic()
      this.scrollingMessageCount = 0
    })

  </script>

</chat>
