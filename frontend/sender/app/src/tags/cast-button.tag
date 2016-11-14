<cast-button>

  <!-- layout -->
  <button class="mdl-button mdl-js-button mdl-button--icon"
          onclick={ state.action }
          if={ state.available }>
    <i class="material-icons">{ state.icon }</i>
  </button>


  <!-- style -->
  <style scoped>

  </style>


  <!-- logic -->
  <script>
    import { SenderEvent } from 'chromecast/sender.js'

    const castIconConnected   = 'cast_connected',
          castIconUnconnected = 'cast'

    function state(available, icon, action) {
      return {
        available: available,
        icon: icon,
        action: action
      }
    }

    const unavailableState = state(false, null,                undefined),
          unconnectedState = state(true,  castIconUnconnected, () => this.sender.connect()),
          connectingState  = state(true,  castIconUnconnected, undefined),
          connectedState   = state(true,  castIconConnected,   () => this.sender.disconnect())

    this.sender = opts.sender
    this.state = unavailableState

    this.onCastInitialized = () => {
      let newStates = {}

      newStates[cast.framework.CastState.NO_DEVICES_AVAILABLE] = unavailableState
      newStates[cast.framework.CastState.NOT_CONNECTED]        = unconnectedState
      newStates[cast.framework.CastState.CONNECTING]           = connectingState
      newStates[cast.framework.CastState.CONNECTED]            = connectedState

      this.sender.on(SenderEvent.CastStateChanged, castState => {
        let newState = newStates[castState]

        this.state = newState
        this.update()
      })
    }

    this.on('mount', () => {
      this.sender.on(SenderEvent.Initialized, this.onCastInitialized)
    })
  </script>

</cast-button>
