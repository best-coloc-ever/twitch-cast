<chromecast-view>

  <!-- layout -->
  <div class="mdl-grid">
    <div class="mdl-layout-spacer"></div>
    <div class="mdl-cell mdl-cell--4-col">
    <h5 align="center">Chromecast Options</h5>
    </div>
    <div class="mdl-layout-spacer"></div>
  </div>

  <div class="mdl-grid">
    <div class="mdl-layout-spacer"></div>
    <div class="mdl-cell mdl-cell--4-col">

      <div each={ option, i in options } class="mdl-grid">
        <div class="mdl-layout-spacer"></div>
        <div class="mdl-cell">
          <label class="mdl-switch mdl-js-switch mdl-js-ripple-effect" for={ 'opt-' + i }>
            <input type="checkbox" id={ 'opt-' + i } class="mdl-switch__input"
                   checked={ option.enabled }
                   onchange={ option.action }>
            <span class="mdl-switch__label">{ option.name }</span>
          </label>
        </div>
        <div class="mdl-layout-spacer"></div>
      </div>

    </div>
    <div class="mdl-layout-spacer"></div>
  </div>

  <!-- style -->
  <style scoped>

  </style>


  <!-- logic -->
  <script>
    import ChromecastMessage from 'chromecast/messages.js'

    let sender = opts.sender

    this.toggleFullscreen = event => {
      console.log(event.target.checked)
      let message = ChromecastMessage.toggleFullscreen(event.target.checked)
      sender.sendCustomMessage(message)
    }

    this.options = [
      { name: 'Fullscreen', enabled: false, action: this.toggleFullscreen },
    ]

    this.on('mount', () => {
      componentHandler.upgradeElements(this.root)
    })
  </script>

</chromecast-view>
