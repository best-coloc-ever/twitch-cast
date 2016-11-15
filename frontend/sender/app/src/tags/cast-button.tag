<cast-button>

  <!-- layout -->
  <div show={ state.available } onclick={ state.action }>
    <div class="mdl-spinner mdl-js-spinner is-active" show={ state.readying }></div>
    <i class="material-icons" show={ !state.readying }>{ state.icon }</i>
  </div>


  <!-- style -->
  <style scoped>
    div {
      cursor: pointer;
    }
  </style>


  <!-- logic -->
  <script>
    import { SenderEvent } from 'chromecast/sender.js'

    const castIconConnected   = 'cast_connected',
          castIconUnconnected = 'cast'

    function state(available, readying, icon, action) {
      return {
        available: available,
        readying: readying,
        icon: icon,
        action: action,
      }
    }

    const unavailableState = state(false, false, null,                undefined),
          unconnectedState = state(true,  false, castIconUnconnected, () => this.sender.connect()),
          connectingState  = state(true,  true,  castIconUnconnected, undefined),
          connectedState   = state(true,  false, castIconConnected,   () => this.sender.disconnect())

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
