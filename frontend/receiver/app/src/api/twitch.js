import { jsonCall } from './helpers.js'

const twitchInit = {
  headers: {
    'Client-ID': TWITCH_CLIENT_ID // Available globally thanks to Webpack
  }
}

function twitchJsonCall(route) {
  let url = `https://api.twitch.tv/kraken${route}`

  return jsonCall(url, twitchInit)
}

function getStream(channelName) {
  return twitchJsonCall(`/streams/${channelName}`)
}

const TwitchAPI = {
  stream: getStream
}

module.exports = TwitchAPI
