<app>

  <!-- Logic -->
  <script>
    import Router from 'routing/router.js'

    import ChromecastReceiver, { ReceiverEvent } from 'chromecast/receiver.js'

    import { PlayerEvent } from 'player/events.js'
    import ChromecastPlayer from 'player/chromecast.js'
    import VideojsPlayer from 'player/videojs.js'

    import { isChromecastDevice } from 'utils/platform.js'

    let initChromecast = (mediaElement) => {
      let receiver = new ChromecastReceiver(mediaElement)

      receiver.on(ReceiverEvent.Ready, () => {
        let player = new ChromecastPlayer(mediaElement)

        startRouting(receiver, player)
      })
    }

    let initNonChromecastDevice = (mediaElement) => {
      let dummyReceiver = riot.observable(),
          player = new VideojsPlayer(mediaElement)

      startRouting(dummyReceiver, player)
    }

    let startRouting = (receiver, player) => {
      let context = {
        receiver: receiver,
        player: player,
      }

      let router = new Router(this.root, context)

      player.on(PlayerEvent.Ready, () => router.start())
    }

    this.on('mount', () => {
      let video = document.createElement('video')
      video.autoplay = true

      let initLogic = (
        isChromecastDevice() ?
        initChromecast :
        initNonChromecastDevice
      )

      initLogic(video)
    })

  </script>

</app>
