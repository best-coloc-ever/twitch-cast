function state(playing, channel, quality) {
  return {
    playing: playing,
    channel: channel,
    quality: quality
  }
}

function mkState(...args) {
  return state.bind(undefined, ...args)
}

export const routes = [
  ['/',    'home-view'   , mkState(false)],
  ['/*',   'loading-view', mkState(false)],
  ['/*/*', 'stream-view' , mkState(true)],
]
