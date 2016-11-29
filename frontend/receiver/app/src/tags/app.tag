<app>

  <!-- Logic -->
  <script>
    import Router, { RouterEvent } from 'routing/router.js'

    import ChromecastReceiver, { ReceiverEvent } from 'chromecast/receiver.js'
    import ChromecastMessage, { ChromecastMessageType } from 'chromecast/messages.js'

    import { PlayerEvent } from 'player/events.js'
    import ChromecastPlayer from 'player/chromecast.js'
    import VideojsPlayer from 'player/videojs.js'

    import { isChromecastDevice } from 'utils/platform.js'

    import { Mixins } from 'context/mixins.js'

    let initChromecast = (mediaElement, router) => new Promise((resolve, _) => {
      let receiver = new ChromecastReceiver(mediaElement),
          appState = null

      let notifyStateChange = () => receiver.sendCustomMessage(
        ChromecastMessage.receiverStateResponse(appState)
      )

      receiver.on(ChromecastMessageType.ReceiverState, notifyStateChange)

      receiver.on(ReceiverEvent.Ready, () => {
        let player = new ChromecastPlayer(mediaElement)

        resolve({ receiver: receiver, player: player })
      })

      router.on(RouterEvent.RouteChanged, (descriptor, path) => {
        appState = descriptor.getState(...path)
        notifyStateChange()
      })
    })

    let initNonChromecastDevice = (mediaElement) => new Promise((resolve, _) => {
      let dummyReceiver = riot.observable(),
          player = new VideojsPlayer(mediaElement)

      resolve({ receiver: dummyReceiver, player: player })
    })

    this.on('mount', () => {
      let router = new Router(this.root),
          video = document.createElement('video')

      video.autoplay = true

      let runApp = context => {
        riot.mixin(Mixins.Receiver, { receiver: context.receiver })
        riot.mixin(Mixins.Player,   { player:   context.player   })

        context.player.on(PlayerEvent.Ready, router.start)
      }

      let initDevice = (
        isChromecastDevice() ?
        initChromecast(video, router) :
        initNonChromecastDevice(video)
      )

      initDevice.then(runApp)
    })

  </script>

</app>
