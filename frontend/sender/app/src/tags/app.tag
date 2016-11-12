<app>

  <!-- layout -->
  <div class="mdl-layout mdl-js-layout mdl-layout--fixed-drawer
              mdl-layout--fixed-header">

    <header class="mdl-layout__header">
      <div class="mdl-layout__header-row">
        <span class="mdl-layout-title">
          <a class="mdl-color-text--primary-contrast" href="#">Twitch Caster</a>
        </span>
      </div>
    </header>

    <nav-bar class="mdl-layout__drawer mdl-color--grey-900"></nav-bar>

    <main class="mdl-layout__content">
      <content-view name="content"></content-view>
    </main>

  </div>


  <!-- style -->
  <style scoped>
    .mdl-layout-title a { text-decoration: none; }

    .mdl-layout__drawer { border: none; }
  </style>


  <!-- logic -->
  <script>
    import { Router } from 'routing/router.js'

    this.on('mount', () => {
      let router = new Router(this.content)

      router.start()
    })
  </script>

</app>
