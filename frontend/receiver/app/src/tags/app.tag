<app>

  <!-- Layout -->
  <div class="box">

    <stream class="flex"></stream>

    <chat show={ chatVisible }></chat>

  </div>

  <!-- Style -->
  <style scoped>
    .box {
      display: flex;
      height: 100%;
    }

    .flex {
      flex: 1 1 auto;
      height: 100%;
      position: relative;
    }

  </style>

  <!-- Logic -->
  <script>
    import ChromecastReceiver from 'receiver/chromecast.js'
    import VideojsReceiver from 'receiver/videojs.js'
    import ReceiverEvent from 'receiver/events.js'

    this.chatVisible = true
    this.receiver = null

    this.on('mount', () => {
      let self = this,
          isChromecastDevice = (navigator.userAgent.indexOf('CrKey') != -1),
          receiverClass = (isChromecastDevice ? ChromecastReceiver : VideojsReceiver),
          mediaElement = this.tags.stream.mediaElement()

      let receiver = new receiverClass(mediaElement)

      receiver.on(ReceiverEvent.ChatToggled, e => {
        self.chatVisible = e.visible
        self.update()
      })

      // TODO: move the chat in the dom (bypass riot or not ?)
      receiver.on(ReceiverEvent.ChatPositionChanged, e => {

      })

      this.receiver = receiver
    })

  </script>

</app>
