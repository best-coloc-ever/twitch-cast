<nav-bar>

  <!-- layout -->
  <a href="#" class="home-link mdl-layout-title mdl-color-text--primary-contrast">
    Twitch Caster
  </a>
  <!-- <hr> -->

  <nav class="mdl-navigation">

    <form name="search-form">
      <div class="mdl-textfield mdl-js-textfield mdl-textfield--expandable search">
        <label class="mdl-button mdl-js-button mdl-button--icon" for="search-input">
          <i class="material-icons mdl-color-text--primary-contrast"">search</i>
        </label>
        <div class="mdl-textfield__expandable-holder">
          <input class="mdl-textfield__input mdl-color-text--primary-contrast" type="text" id="search-input" placeholder="Search">
          <label class="mdl-textfield__label"></label>
        </div>
      </div>
    </form>

    <hr class="category-separator">
    <div each={ links in linksCollection }>
      <a each={ link in links }
         class={
           "mdl-navigation__link": true,
           "mdl-color-text--primary-contrast": true,
           "active": (activeRoute == link.route)
          }
         show={ link.visibleIf() }
         href={ '#' + link.route }>
        <i class="material-icons">{ link.icon }</i><span>{ link.display }</span>
      </a>
    </div>

    <hr class="category-separator" show={ !oauthToken }>

    <button show={ !oauthToken } class="mdl-button mdl-js-button mdl-color-text--primary-contrast mdl-button--colored mdl-button--raised" onclick={ login }>Log in with Twitch</button>

  </nav>

  <!-- style -->
  <style scoped>
    .home-link {
      text-decoration: none;
    }

    .search {
      padding: 20px 20px;
    }

    .category {
      background-color: #404040;
      padding: 4px 40px;
    }

    .category-separator {
      margin: 0
    }

    .material-icons {
      padding-right: 10px;
    }

    .mdl-navigation__link:hover {
      background-color: #673ab7 !important;
    }

    .active {
      background-color: #101010 !important;
    }

    .active i, .active span {
      color: #b186ff;
    }
  </style>

  <!-- logic -->
  <script>
    import { routeNames } from 'routing/routes.js'
    import { SenderEvent } from 'chromecast/sender.js'

    import Cookies from 'js-cookie'

    import TwitchAPI from 'api/twitch.js'

    const link = ([route, icon, display, visibleIf]) => new Object({
      route: route,
      icon: icon,
      display: display,
      visibleIf: visibleIf
    })
    const linkDescriptors = [
      [
        [routeNames.Channels,   'videocam', 'Channels',   () => true],
        [routeNames.Games,      'gamepad',  'Games',      () => true],
      ],
      [
        [routeNames.Following,  'favorite', 'Following',  () => Cookies.get('twitch-oauth-token')]
      ],
      [
        [routeNames.Chromecast, 'cast',     'Chromecast', () => this.senderConnected]
      ]
    ]

    this.linksCollection = linkDescriptors.map(links => links.map(link))
    this.activeRoute = null
    this.senderConnected = false
    this.oauthToken = Cookies.get('twitch-oauth-token')

    this.hideDrawer = () => {
      let layout = document.querySelector('.mdl-layout')
      let drawer = this.root

      if (drawer.classList.contains('is-visible'))
        layout.MaterialLayout.toggleDrawer()
    }

    this.login = () => {
      window.location = TwitchAPI.OAuth.authorizeUrl()
    }

    this.on('mount', () => {

      this.linksCollection.forEach(links => {
        links.forEach(link => {
          riot.route(`/${link.route}`, () => {
            this.hideDrawer()
            this.activeRoute = link.route
            this.parent.updateTitle(link.display)
            this.update()
          })
        })
      })

      this['search-form'].onsubmit = e => {
        this.hideDrawer()
        riot.route(`/search/${this['search-input'].value}`)
        return false
      }

      opts.sender.on(SenderEvent.CastStateChanged, state => {
        this.senderConnected = (state == cast.framework.CastState.CONNECTED)
        this.update()
      })

      riot.route('/twitch-oauth..', () => {
        let code = riot.route.query().code

        TwitchAPI.OAuth.token(code)
          .then(data => {
            this.oauthToken = data.access_token
            Cookies.set('twitch-oauth-token', data.access_token)
            riot.route('/following')
          })
      })

    })
  </script>

</nav-bar>
