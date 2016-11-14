export const routeNames = {
  Channels: 'channels',
  Games:    'games'
}

export const routes = [
  ['/',                       'home-view'    ],
  [`/${routeNames.Channels}`, 'channels-view'],
  [`/${routeNames.Games}`,    'games-view'   ],
]
