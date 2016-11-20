<chromecast-view>

  <!-- layout -->
  <div class="mdl-grid" show={ receiverState }>
    <div class="mdl-layout-spacer"></div>

    <div class="mdl-cell mdl-cell--3-col" show={ receiverState.playing }>
      <h5 class="title">Playing - { receiverState.channel }</h5>

      <hr>

      <div name="quality-option" show={ qualities }>
        <button id="quality-drop-down" class="mdl-button mdl-js-button mdl-button--icon">
          <i class="material-icons">more_vert</i>
        </button>
        <span class="label">Quality</span>
        <ul class="mdl-menu mdl-menu--bottom-left mdl-js-menu mdl-js-ripple-effect"
            data-mdl-for="quality-drop-down"
            name="quality-list">
          <li each={ quality in qualities } class="mdl-menu__item" onclick={ changeQuality(quality) }>
            { quality.toLowerCase() }
          </li>
        </ul>
      </div>

      <hr>

      <div name="fullscreen-option">
        <label class="mdl-switch mdl-js-switch mdl-js-ripple-effect" for="opt-fullscreen">
          <input type="checkbox" id="opt-fullscreen" class="mdl-switch__input"
                 onchange={ toggleFullscreen }>
          <span class="mdl-switch__label">Fullscreen</span>
        </label>
      </div>

      <hr>

      <div name="chat-position-option">
      <button id="chat-position-drop-down" class="mdl-button mdl-js-button mdl-button--icon">
          <i class="material-icons">more_vert</i>
        </button>
        <span class="label">Chat position</span>
        <ul class="mdl-menu mdl-menu--bottom-left mdl-js-menu mdl-js-ripple-effect"
            data-mdl-for="chat-position-drop-down"
            name="quality-list">
          <li each={ position in chatPositions }
              class="mdl-menu__item" onclick={ changeChatPosition(position) }>
            { position.toLowerCase() }
          </li>
        </ul>
      </div>

    </div>

    <div class="mdl-cell mdl-cell--4-col" show={ !receiverState.playing }>
      <h5>Not playing anything at the moment</h5>
    </div>

    <div class="mdl-layout-spacer"></div>
  </div>

  </div>

  <div show={ !receiverState }>
    <h4 align="center">{ statusMessage }</h4>
  </div>

  <!-- style -->
  <style scoped>
    .title {
      margin-bottom: 30px;
      margin-left: 10px;
    }

    span {
      font-size: 16px;
    }

    .label {
      margin-left: 16px;
    }
  </style>


  <!-- logic -->
  <script>
    import { SenderEvent } from 'chromecast/sender.js'
    import ChromecastMessage, { ChromecastMessageType, ChatPositions } from 'chromecast/messages.js'
    import StreamerAPI from 'api/streamer.js'

    let sender = opts.sender

    this.receiverState = null
    this.statusMessage = 'Waiting for a resumed session...'
    this.qualities = null
    this.chatPositions = Object.values(ChatPositions)
    console.log(this.chatPositions)

    this.setStatus = message => {
      this.statusMessage = message
      this.update()
    }

    this.toggleFullscreen = event => {
      let message = ChromecastMessage.toggleFullscreen(event.target.checked)

      sender.sendCustomMessage(message)
    }

    this.changeQuality = quality => () => {
      sender.play(this.receiverState.channel, quality)
    }

    this.changeChatPosition = position => () => {
      let message = ChromecastMessage.chatPosition(position)

      sender.sendCustomMessage(message)
    }

    this.onStateRequestComplete = mbError => {
      let status = mbError || 'Waiting for a response from the receiver...'

      this.setStatus(status)
    }

    this.onStateRequestError = (e) => {
      this.setStatus('Could not reach the receiver')
    }

    this.initialize = () => {
      this.setStatus('Connecting to the receiver...')

      sender.sendCustomMessage(ChromecastMessage.receiverStateRequest())
        .then(this.onStateRequestComplete, this.onStateRequestError)
    }

    this.fetchQualities = (channel) => {
      StreamerAPI.stream(channel)
        .then(data => {
          this.qualities = data.playlists.map(pl => pl.name)
          this.update()
          componentHandler.upgradeElements(this['quality-option'])
        })
    }

    this.on('mount', () => {
      componentHandler.upgradeElements(this['fullscreen-option'])
      componentHandler.upgradeElements(this['chat-position-option'])

      sender.on(ChromecastMessageType.ReceiverState, state => {
        this.receiverState = state

        if (state.quality)
          this.fetchQualities(state.channel)

        this.update()
      })

      if (sender.connected())
        this.initialize()
      else
        setTimeout(this.initialize, 1000) // Letting time to the sender to resume an possible session
    })
  </script>

</chromecast-view>
