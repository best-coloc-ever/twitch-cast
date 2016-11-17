<app>

  <!-- Logic -->
  <script>
    import Router from 'routing/router.js'

    import ChromecastReceiver from 'chromecast/receiver.js'

    import { PlayerEvent } from 'player/events.js'
    import ChromecastPlayer from 'player/chromecast.js'
    import VideojsPlayer from 'player/videojs.js'

    import { isChromecastDevice } from 'utils/platform.js'

    this.on('mount', () => {
      let video = document.createElement('video')
      video.autoplay = true

      let [receiver, player] = (
        isChromecastDevice() ?
        [new ChromecastReceiver, new ChromecastPlayer(video)] :
        [riot.observable(),      new VideojsPlayer(video)   ]
      )

      let router = new Router(this.root, { receiver: receiver, player: player, video: video })

      player.on(PlayerEvent.Ready, () => router.start())
    })

  </script>

</app>
