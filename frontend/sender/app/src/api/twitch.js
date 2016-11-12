import { jsonCall } from './helpers.js'

const twitchInit = {
  headers: {
    'Client-ID': TWITCH_CLIENT_ID
  }
}

function twitchJsonCall(route, params={}) {
  let url = new URL(`https://api.twitch.tv/kraken${route}`)

  Object.keys(params).forEach(key => {
    url.searchParams.append(key, params[key])
  })

  return jsonCall(url, twitchInit)
}

export const TwitchAPI = {
  streams: (params={}) => twitchJsonCall('/streams', params)
}
