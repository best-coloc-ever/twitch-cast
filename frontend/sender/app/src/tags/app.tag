<app>

  <!-- layout -->
  <div class="mdl-layout mdl-js-layout mdl-layout--fixed-drawer
              mdl-layout--fixed-header">

    <header class="mdl-layout__header">
      <div class="mdl-layout__header-row">
        <span class="mdl-layout-title">
          <a class="mdl-color-text--primary-contrast" href="#">{ title }</a>
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
    import ChromecastSender from 'chromecast/sender.js'
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

      router.start()
    })
  </script>

</app>
