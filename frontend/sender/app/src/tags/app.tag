<app>

  <!-- layout -->
  <div class="mdl-layout mdl-js-layout mdl-layout--fixed-drawer
              mdl-layout--fixed-header">

    <layout-header class="mdl-layout__header"></layout-header>

    <navbar class="mdl-layout__drawer mdl-color--grey-900"></navbar>

    <main class="mdl-layout__content">
      <content-view ref="content"></content-view>
    </main>

    <snackbar></snackbar>

  </div>


  <!-- style -->
  <style scoped>
    .mdl-layout__drawer { border: none; }
  </style>


  <!-- logic -->
  <script>
    import { ChromecastSender, SenderEvent } from 'chromecast/sender.js'
    import { Router } from 'routing/router.js'
    import { Mixins } from 'context/mixins.js'

    this.on('mount', () => {
      let sender = new ChromecastSender,
          router = new Router(this.refs.content)

      riot.mixin(Mixins.Sender, { sender: sender })
      riot.mixin(Mixins.Router, { router: router })

      sender.on(SenderEvent.Initialized, router.start)
    })
  </script>

</app>
