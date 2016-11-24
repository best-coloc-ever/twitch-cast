export const routeDescriptors = {
  Home: {
    base: '',
    tagNames: ['home-view']
  },

  Channels: {
    base: 'channels',
    tagNames: ['channels-view']
  },

  Games: {
    base: 'games',
    tagNames: ['games-view', 'channels-view']
  },

  Search: {
    base: 'search',
    tagNames: [undefined, 'search-view']
  },

  Chromecast: {
    base: 'chromecast',
    tagNames: ['chromecast-view']
  },

  Following: {
    base: 'following',
    tagNames: ['following-view']
  }
}

const routeLink = (descriptor, ...path) =>
  [descriptor.base, ...path].map(encodeURIComponent).join('/')

const r = routeDescriptors

export const routeLinks = {
  home:       ()    => routeLink(r.Home            ),
  channels:   ()    => routeLink(r.Channels        ),
  games:      ()    => routeLink(r.Games           ),
  game:       name  => routeLink(r.Games,     name ),
  search:     query => routeLink(r.Search,    query),
  chromecast: ()    => routeLink(r.Chromecast      ),
  following:  ()    => routeLink(r.Following       )
}
