import { jsonCall } from './helpers.js'

const twitchInit = {
  headers: {
    'Client-ID': TWITCH_CLIENT_ID // Available globally thanks to Webpack
  }
}

function twitchJsonCall(route) {
  let url = `//api.twitch.tv/kraken${route}`

  return jsonCall(url, twitchInit)
}

const TwitchAPI = {
  stream: (channelName) => twitchJsonCall(`/streams/${channelName}`),
  badges: (channelName) => twitchJsonCall(`/chat/${channelName}/badges`)
}

module.exports = TwitchAPI
