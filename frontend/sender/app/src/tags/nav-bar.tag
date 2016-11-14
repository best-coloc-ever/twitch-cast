<nav-bar>

  <!-- layout -->
  <span class="mdl-layout-title mdl-color-text--primary-contrast">
    Browse
  </span>

  <nav class="mdl-navigation">

    <a each={ link in links }
       class={
         "mdl-navigation__link": true,
         "mdl-color-text--primary-contrast": true,
         "active": (activeRoute == link.route)
        }
       href={ '#' + link.route }>
      <i class="material-icons">{ link.icon }</i><span>{ link.display }</span>
    </a>
  </nav>

  <!-- style -->
  <style scoped>
    .material-icons {
      padding-right: 10px;
    }

    .mdl-navigation__link:hover {
      background-color: #673ab7 !important;
    }

    .active {
      background-color: #673ab7 !important;
    }
  </style>

  <!-- logic -->
  <script>
    import { routeNames } from 'routing/routes.js'

    const link = ([route, icon, display]) => new Object({
      route: route,
      icon: icon,
      display: display
    })
    const linkDescriptors = [
      [routeNames.Channels, 'videocam', 'Channels'],
      [routeNames.Games,    'gamepad',  'Games'   ],
    ]

    this.links = linkDescriptors.map(link)
    this.activeRoute = null

    this.on('mount', () => {

      linkDescriptors.forEach(([route]) => {
        riot.route(`/${route}`, () => {
          this.activeRoute = route
          this.update()
        })
      })

    })
  </script>

</nav-bar>
