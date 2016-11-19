<app>

  <!-- layout -->
  <div class="mdl-layout mdl-js-layout mdl-layout--fixed-drawer
              mdl-layout--fixed-header">

    <header class="mdl-layout__header">
      <div class="mdl-layout__header-row">
        <span class="mdl-layout-title">{ title }</span>
        <div class="mdl-layout-spacer"></div>
        <nav class="mdl-navigation">
          <cast-button sender={ sender }></cast-button>
        </nav>
      </div>
    </header>

    <nav-bar class="mdl-layout__drawer mdl-color--grey-900" sender={ sender }></nav-bar>

    <main class="mdl-layout__content">
      <content-view name="content"></content-view>
    </main>

    <snackbar sender={ sender } app={ this }></snackbar>

  </div>


  <!-- style -->
  <style scoped>
    .mdl-layout__drawer { border: none; }
  </style>


  <!-- logic -->
  <script>
    import ChromecastSender, { SenderEvent } from 'chromecast/sender.js'
    import { Router } from 'routing/router.js'

    this.title = null
    this.sender = new ChromecastSender

    this.updateTitle = title => {
      this.title = title
      this.update()
    }

    this.on('mount', () => {
      let context = {
        app: this,
        sender: this.sender
      }

      let router = new Router(this.content, context)

      this.sender.on(SenderEvent.Initialized, router.start)
    })
  </script>

</app>
