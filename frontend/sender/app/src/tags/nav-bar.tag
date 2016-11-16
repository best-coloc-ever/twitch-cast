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
          <input class="mdl-textfield__input" type="text" id="search-input" placeholder="Search">
          <label class="mdl-textfield__label"></label>
        </div>
      </div>
    </form>

    <!-- <hr>
    <div class="category mdl-color-text--primary-contrast">Browse</div> -->
    <hr class="category-separator">
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

      linkDescriptors.forEach(([route, _, title]) => {
        riot.route(`/${route}`, () => {
          this.activeRoute = route
          this.parent.updateTitle(title)
          this.update()
        })
      })

      this['search-form'].onsubmit = e => {
        riot.route(`/search/${this['search-input'].value}`)

        return false
      }

    })
  </script>

</nav-bar>
