<app>

  <!-- layout -->
  <div class="mdl-layout mdl-js-layout mdl-layout--fixed-drawer
              mdl-layout--fixed-header">

    <header class="mdl-layout__header">
      <div class="mdl-layout__header-row">
        <span class="mdl-layout-title">{ title }</span>
        <div class="mdl-layout-spacer"></div>

        <form name="search-form" class="layout-header-action">
          <div class="mdl-textfield mdl-js-textfield mdl-textfield--expandable search" name="expandable-search">
            <label class="mdl-button mdl-js-button mdl-button--icon" for="search-input">
              <i class="material-icons mdl-color-text--primary-contrast"">search</i>
            </label>
            <div class="mdl-textfield__expandable-holder">
              <input class="mdl-textfield__input mdl-color-text--primary-contrast" type="text" id="search-input" placeholder="Search">
              <label class="mdl-textfield__label"></label>
            </div>
          </div>
        </form>

        <cast-button sender={ sender } class="layout-header-action"></cast-button>
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

    .layout-header-action { margin-left: 15px; }
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
      this['search-form'].onsubmit = e => {
        let query = this['search-input'].value
        this['expandable-search'].classList.remove('is-dirty')

        this.updateTitle(query)
        riot.route(`/search/${query}`)

        return false
      }

      let context = {
        app: this,
        sender: this.sender
      }

      let router = new Router(this.content, context)

      this.sender.on(SenderEvent.Initialized, router.start)
    })
  </script>

</app>
