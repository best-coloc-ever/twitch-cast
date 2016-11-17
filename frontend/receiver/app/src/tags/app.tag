<app>

  <!-- Logic -->
  <script>
    import Router from 'routing/router.js'

    import ChromecastReceiver from 'chromecast/receiver.js'
    import { isChromecastDevice } from 'utils/platform.js'

    this.on('mount', () => {
      let receiver = (
        isChromecastDevice() ?
        new ChromecastReceiver :
        riot.observable()
      )

      let router = new Router(this.root, { receiver: receiver })

      router.start()
    })

  </script>

</app>
