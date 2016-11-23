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
    import { Mixins } from 'context/mixins.js'

    this.mixin(Mixins.Sender)

    this.onChannelSent = channel => {
      let channelName = channel.toUpperCase()
      let device = this.sender.deviceName().toUpperCase()

      this.show({
        message: `${channelName} is now playing on ${device}`,
        actionText: 'Watch on this device',
        actionHandler: (..._) => this.sender.playLocally(channel),
        timeout: 8 * 1000
      })
    }

    this.onChannelQueued = channel => {
      let channelName = channel.toUpperCase()

      this.show({
        message: `Connecting to cast device - ${channelName} will play shortly`
      })
    }

    this.onCastError = error => {
      this.show({ message: error })
    }

    this.show = data => {
      this.snackbar.MaterialSnackbar.showSnackbar(data)
    }

    this.on('mount', () => {
      this.sender.on(SenderEvent.ChannelSent, this.onChannelSent)
      this.sender.on(SenderEvent.ChannelQueued, this.onChannelQueued)
      this.sender.on(SenderEvent.CastError, this.onCastError)

    })
  </script>

</snackbar>
