<snackbar>

  <!-- layout -->
  <div class="mdl-snackbar mdl-js-snackbar" name="snackbar">
    <div class="mdl-snackbar__text"></div>
    <button class="mdl-snackbar__action" type="button"></button>
  </div>


  <!-- style -->
  <style scoped>

  </style>


  <!-- logic -->
  <script>
    import { SenderEvent } from 'chromecast/sender.js'

    this.onChannelSent = channel => {
      this.show({
        message: `${channel.toUpperCase()} is now playing on ${opts.sender.deviceName().toUpperCase()}`,
        actionHandler: (..._) => opts.app.playLocally(channel),
        actionText: 'Watch on this device',
        timeout: 3 * 1000
      })
    }

    this.show = data => {
      this.snackbar.MaterialSnackbar.showSnackbar(data)
    }

    this.on('mount', () => {
      opts.sender.on(SenderEvent.ChannelSent, this.onChannelSent)

    })
  </script>

</snackbar>
