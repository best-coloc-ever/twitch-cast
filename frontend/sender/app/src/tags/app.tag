<app>

  <!-- layout -->
  <div class="mdl-layout mdl-js-layout mdl-layout--fixed-drawer
              mdl-layout--fixed-header">

    <header class="mdl-layout__header">
      <div class="mdl-layout__header-row">
        <span class="mdl-layout-title">
          <a class="mdl-color-text--primary-contrast" href="#">Twitch Caster</a>
        </span>
        <div class="mdl-layout-spacer"></div>
        <nav class="mdl-navigation">
          <cast-button sender={ sender }></cast-button>
        </nav>
      </div>

    </header>

    <nav-bar class="mdl-layout__drawer mdl-color--grey-900"></nav-bar>

    <main class="mdl-layout__content">
      <content-view name="content"></content-view>
    </main>

    <snackbar sender={ sender } app={ this }></snackbar>

  </div>


  <!-- style -->
  <style scoped>
    .mdl-layout-title a { text-decoration: none; }

    .mdl-layout__drawer { border: none; }
  </style>


  <!-- logic -->
  <script>
    import { Router } from 'routing/router.js'
    import ChromecastSender from 'chromecast/sender.js'
    import StreamerAPI from 'api/streamer.js'

    this.sender = new ChromecastSender

    this.playLocally = channel => {
      let url = StreamerAPI.receiverUrl(channel)
      window.location.replace(url)
    }

    this.on('mount', () => {
      riot.route('/watch/*', channel => {
        if (!this.sender.connected())
          this.playLocally(channel)
      })

      let router = new Router(this.content)

      router.start()
    })
  </script>

</app>
