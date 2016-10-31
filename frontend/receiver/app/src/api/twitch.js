import { jsonCall, jsonPCall } from './helpers.js'

const twitchInit = {
  headers: {
    'Client-ID': TWITCH_CLIENT_ID // Available globally thanks to Webpack
  }
}

function twitchJsonCall(route) {
  let url = `//api.twitch.tv/kraken${route}`

  return jsonCall(url, twitchInit)
}

const betaBadgesEndpointPrefix = '//badges.twitch.tv/v1/badges'

const TwitchAPI = {
  channel: (channelName) => twitchJsonCall(`/channels/${channelName}`),
  stream: (channelName) => twitchJsonCall(`/streams/${channelName}`),
  badges: (channelName) => twitchJsonCall(`/chat/${channelName}/badges`),

  Beta: {
    badges: () => jsonPCall(`${betaBadgesEndpointPrefix}/global/display`),
    channelBadges: (channelId) => jsonPCall(`${betaBadgesEndpointPrefix}/channels/${channelId}/display`)
  }
}

module.exports = TwitchAPI
