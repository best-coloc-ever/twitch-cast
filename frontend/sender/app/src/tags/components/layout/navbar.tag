<navbar>

  <!-- layout -->
  <github-corner></github-corner>

  <div class="title-container mdl-layout-title">
    <a href="#" class="home-link mdl-color-text--primary-contrast">
      Twitch Caster
    </a>
  </div>

  <nav class="mdl-navigation">
    <hr class="category-separator">

    <div>
      <a each={ link in links }
         class="mdl-navigation__link mdl-color-text--primary-contrast
                { active: (activeRoute == link.route) }"
         show={ link.visibleIf() }
         href={ '#' + link.route }>
        <i class="material-icons">{ link.icon }</i><span>{ link.display }</span>
      </a>
    </div>

    <hr class="category-separator separator-spaced-bottom">

    <twitch-login-button></twitch-login-button>
  </nav>

  <!-- style -->
  <style scoped>
    github-corner {
      z-index: 10;
      position: absolute;
      width: 64px;
      height: 64px;
    }

    .title-container {
      display: flex;
      white-space: nowrap;
      margin-top: 50px;
    }

    .home-link {
      text-decoration: none;
    }

    .category-separator {
      margin: 0;
      height: 0;
      border-top: 2px solid #673ab7;
      border-bottom: 2px solid #673ab7;
    }

    .separator-spaced-bottom {
      margin-bottom: 10px;
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

    twitch-login-button > * {
      width: 100%;
    }
  </style>

  <!-- logic -->
  <script>
    import { routeDescriptors } from 'routing/routes.js'
    import { RouterEvent } from 'routing/router.js'
    import { SenderEvent } from 'chromecast/sender.js'
    import { Mixins } from 'context/mixins.js'
    import { object } from 'utils/prelude.js'

    import TwitchAPI from 'api/twitch.js'

    const r          = routeDescriptors,
          link       = object('route',           'icon',     'display',    'visibleIf'               ),
          channels   = link  (r.Channels.base,   'videocam', 'Channels',   () => true                ),
          games      = link  (r.Games.base,      'gamepad',  'Games',      () => true                ),
          following  = link  (r.Following.base,  'favorite', 'Following',  TwitchAPI.isAuthenticated ),
          chromecast = link  (r.Chromecast.base, 'cast',     'Chromecast', () => this.senderConnected)

    this.mixin(Mixins.Sender)
    this.mixin(Mixins.Router)
    this.links = [channels, games, following, chromecast]
    this.activeRoute = null
    this.senderConnected = false

    this.hideDrawer = () => {
      let layout = document.querySelector('.mdl-layout')
      let drawer = this.root

      if (drawer.classList.contains('is-visible'))
        layout.MaterialLayout.toggleDrawer()
    }

    this.router.on(RouterEvent.RouteChanged, routeDescriptor => {
      this.hideDrawer()
      this.update({ activeRoute: routeDescriptor.base })
    })

    this.sender.on(SenderEvent.CastStateChanged, state => {
      let isConnected = (state == cast.framework.CastState.CONNECTED)
      this.update({ senderConnected: isConnected })
    })
  </script>

</navbar>
