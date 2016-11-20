export const routeNames = {
  Channels:   'channels',
  Games:      'games',
  Search:     'search',
  Chromecast: 'chromecast',
  Following:  'following'
}

export const routes = [
  ['/',                         'home-view'      ],
  [`/${routeNames.Channels}`,   'channels-view'  ],
  [`/${routeNames.Games}`,      'games-view'     ],
  [`/${routeNames.Games}/*`,    'channels-view'  ],
  [`/${routeNames.Search}/*`,   'search-view'    ],
  [`/${routeNames.Chromecast}`, 'chromecast-view'],
  [`/${routeNames.Following}`,  'following-view' ],
]
