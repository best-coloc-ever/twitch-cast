<chromecast-view>

  <!-- layout -->
  <div class="mdl-grid" if={ receiverState }>
    <div class="mdl-layout-spacer"></div>

    <div class="mdl-cell mdl-cell--3-col" show={ receiverState.playing }>
      <h5 class="title">Playing - { receiverState.channel }</h5>

      <hr>

      <div class="mdl-spinner mdl-js-spinner is-active spinner-white"
           show={ !qualities }
           ref="quality-spinner"></div>
      <div ref="quality-option" show={ qualities }>
        <button id="quality-drop-down" class="mdl-button mdl-js-button mdl-button--icon">
          <i class="material-icons">more_vert</i>
        </button>
        <span class="label">Quality</span>
        <ul class="mdl-menu mdl-menu--bottom-left mdl-js-menu mdl-js-ripple-effect"
            data-mdl-for="quality-drop-down">
          <li each={ quality in qualities } class="mdl-menu__item" onclick={ changeQuality(quality) }>
            { quality.toLowerCase() }
          </li>
        </ul>
      </div>

      <hr>

      <div ref="fullscreen-option">
        <label class="mdl-switch mdl-js-switch mdl-js-ripple-effect" for="opt-fullscreen">
          <input type="checkbox" id="opt-fullscreen" class="mdl-switch__input"
                 onchange={ toggleFullscreen }>
          <span class="mdl-switch__label">Fullscreen</span>
        </label>
      </div>

      <hr>

      <div ref="chat-position-option">
        <button id="chat-position-drop-down" class="mdl-button mdl-js-button mdl-button--icon">
          <i class="material-icons">more_vert</i>
        </button>
        <span class="label">Chat position</span>
        <ul class="mdl-menu mdl-menu--bottom-left mdl-js-menu mdl-js-ripple-effect"
            data-mdl-for="chat-position-drop-down">
          <li each={ position in chatPositions }
              class="mdl-menu__item" onclick={ changeChatPosition(position) }>
            { position.toLowerCase() }
          </li>
        </ul>
      </div>

      <hr>

      <div ref="chat-size-option">
        <span class="label-only">Chat size: { chatSize }px</span>
        <input class="mdl-slider mdl-js-slider" type="range" ref="slider"
               min="0" max="900" value="300" onchange={ changeChatSize }>
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

    .label-only {
      margin-left: 54px;
    }
  </style>


  <!-- logic -->
  <script>
    import { SenderEvent } from 'chromecast/sender.js'
    import ChromecastMessage, { ChromecastMessageType, ChatPositions } from 'chromecast/messages.js'
    import StreamerAPI from 'api/streamer.js'
    import { Mixins } from 'context/mixins.js'

    this.receiverState = null
    this.statusMessage = 'Waiting for a resumed session...'
    this.qualities = null
    this.chatPositions = Object.values(ChatPositions)
    this.chatSize = 300

    this.toggleFullscreen = event => {
      let message = ChromecastMessage.toggleFullscreen(event.target.checked)

      this.sender.sendCustomMessage(message)
    }

    this.changeQuality = quality => () => {
      this.sender.play(this.receiverState.channel, quality)
    }

    this.changeChatPosition = position => () => {
      let message = ChromecastMessage.chatPosition(position)

      this.sender.sendCustomMessage(message)
    }

    this.changeChatSize = event => {
      let chatSize = event.target.valueAsNumber,
          message = ChromecastMessage.chatSize(chatSize)

      this.sender.sendCustomMessage(message)
      this.update({ chatSize: chatSize })
    }

    this.onStateRequestComplete = mbError => {
      let status = mbError || 'Waiting for a response from the receiver...'

      this.update({ statusMessage: status })
    }

    this.onStateRequestError = (e) => {
      this.update({ statusMessage: 'Could not reach the receiver' })
    }

    this.initialize = () => {
      this.update({ statusMessage: 'Connecting to the receiver...' })

      this.sender.sendCustomMessage(ChromecastMessage.receiverStateRequest())
        .then(this.onStateRequestComplete, this.onStateRequestError)
    }

    this.fetchQualities = (channel) => {
      StreamerAPI.stream(channel)
        .then(data => {
          this.update({ qualities: data.playlists.map(pl => pl.name) })
          componentHandler.upgradeElements(this.refs['quality-option'])
        })
    }

    this.onStateChanged = (state) => {
      this.update({ receiverState: state })

      componentHandler.upgradeElements(this.refs['fullscreen-option'])
      componentHandler.upgradeElements(this.refs['chat-position-option'])
      componentHandler.upgradeElements(this.refs['chat-size-option'])
      componentHandler.upgradeElements(this.refs['quality-spinner'])

      if (state.quality)
        this.fetchQualities(state.channel)
    }

    this.on('mount', () => {
      this.mixin(Mixins.Sender)

      this.sender.on(ChromecastMessageType.ReceiverState, this.onStateChanged)

      if (this.sender.connected())
        this.initialize()
      else
        setTimeout(this.initialize, 1000) // Letting time to the sender to resume an possible session
    })

    this.on('unmount', () => {
      this.sender.off(ChromecastMessageType.ReceiverState, this.onStateChanged)
    })
  </script>

</chromecast-view>
