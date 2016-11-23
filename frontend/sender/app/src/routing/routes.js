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
