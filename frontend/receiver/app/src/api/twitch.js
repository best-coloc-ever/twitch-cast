import { jsonCall, jsonPCall } from './helpers.js'

const twitchInit = {
  headers: {
    'Client-ID': TWITCH_CLIENT_ID // Available globally thanks to Webpack
  }
}

function twitchJsonCall(route, params={}) {
  let url = `//api.twitch.tv/kraken${route}`

  return jsonCall(url, { init: twitchInit, params: params })
}

const betaBadgesEndpointPrefix = '//badges.twitch.tv/v1/badges'

const TwitchAPI = {
  channel: (channelName) => twitchJsonCall(`/channels/${channelName}`),
  stream:  (channelName) => twitchJsonCall(`/streams/${channelName}`),
  streams: (params = {}) => twitchJsonCall('/streams', params),
  badges:  (channelName) => twitchJsonCall(`/chat/${channelName}/badges`),

  Beta: {
    badges:        ()          => jsonPCall(`${betaBadgesEndpointPrefix}/global/display`),
    channelBadges: (channelId) => jsonPCall(`${betaBadgesEndpointPrefix}/channels/${channelId}/display`)
  }
}

export default TwitchAPI
