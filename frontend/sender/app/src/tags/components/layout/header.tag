<layout-header>

  <div class="mdl-layout__header-row">
    <span class="mdl-layout-title">{ title }</span>
    <virtual each={ component in path }>
      <i class="material-icons">keyboard_arrow_right</i>
      <span class="path-component">{ component }</span>
    </virtual>

    <div class="mdl-layout-spacer"></div>

    <search-button></search-button>
    <cast-button class="layout-header-action"></cast-button>
  </div>


  <!-- style -->
  <style scoped>
    cast-button { margin-left: 10px }

    .path-component {
      font-size: 18px;
    }
  </style>


  <!-- logic -->
  <script>
    import { Mixins } from 'context/mixins.js'
    import { RouterEvent } from 'routing/router.js'
    import { routeDescriptors } from 'routing/routes.js'

    const r = routeDescriptors,
          titlesSpec = [
            [r.Home,       'Home'             ],
            [r.Channels,   'Channels'         ],
            [r.Games,      'Games'            ],
            [r.Search,     'Search'           ],
            [r.Chromecast, 'Chromecast remote'],
            [r.Following,  'Following'        ],
          ],
          titlesMap = new Map(titlesSpec.map(([k, v]) => [k.base, v]))

    this.mixin(Mixins.Router)
    this.title = null
    this.path = []

    this.router.on(RouterEvent.RouteChanged, (routeDescriptor, path) => {
      this.update({
        title: titlesMap.get(routeDescriptor.base),
        path: path
      })
    })
  </script>

</layout-header>
