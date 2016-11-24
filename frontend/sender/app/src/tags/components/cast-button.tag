<cast-button>

  <!-- layout -->
  <div show={ state.available } onclick={ state.action }>
    <div class="mdl-spinner mdl-js-spinner is-active spinner-white" show={ state.readying }></div>
    <i class="material-icons" show={ !state.readying }>{ state.icon }</i>
  </div>


  <!-- style -->
  <style scoped>
    :scope {
      cursor: pointer;
    }

    .spinner-white .mdl-spinner__layer {
      border-color: white;
    }
  </style>


  <!-- logic -->
  <script>
    import { SenderEvent } from 'chromecast/sender.js'
    import { Mixins } from 'context/mixins.js'
    import { object } from 'utils/prelude.js'

    const castIconConnected   = 'cast_connected',
          castIconUnconnected = 'cast'

    const state       = object('available', 'readying', 'icon',               'action'                      ),
          unavailable = state (false,       false,       null,                undefined                     ),
          unconnected = state (true,        false,       castIconUnconnected, () => this.sender.connect()   ),
          connecting  = state (true,        true,        castIconUnconnected, undefined                     ),
          connected   = state (true,        false,       castIconConnected,   () => this.sender.disconnect())

    this.state = unavailable

    this.onCastInitialized = () => {
      const cs = cast.framework.CastState,
            statesSpec = [
              [cs.NO_DEVICES_AVAILABLE, unavailable],
              [cs.NOT_CONNECTED,        unconnected],
              [cs.CONNECTING,           connecting ],
              [cs.CONNECTED,            connected  ],
            ],
            stateMap = new Map(statesSpec)

      this.sender.on(SenderEvent.CastStateChanged, castState => {
        this.update({ state: stateMap.get(castState) })
      })
    }

    this.on('mount', () => {
      this.mixin(Mixins.Sender)

      this.sender.on(SenderEvent.Initialized, this.onCastInitialized)
    })
  </script>

</cast-button>
