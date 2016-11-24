import { jsonCall, jsonPCall } from './helpers.js'

import Cookies from 'js-cookie'

const twitchInit = {
  headers: {
    'Client-ID': TWITCH_CLIENT_ID // Available globally thanks to Webpack
  }
}

const oauthInit = token => {
  return {
    headers: {
      'Authorization': `OAuth ${token}`
    }
  }
}

function twitchJsonCall(route, params={}) {
  let url = `//api.twitch.tv/kraken${route}`

  return jsonCall(url, { init: twitchInit, params: params })
}

function twitchJsonCallOauth(route, token, params={}) {
  let url = `//api.twitch.tv/kraken${route}`

  return jsonCall(url, { init: oauthInit(token), params: params })
}

const betaBadgesEndpointPrefix = '//badges.twitch.tv/v1/badges'

const OAuthAuthorizeUrl = 'https://api.twitch.tv/kraken/oauth2/authorize',
      OAuthTokenUrl = 'https://api.twitch.tv/kraken/oauth2/token',
      OAuthTokenCookieName = 'twitch-oauth-token'

const TwitchAPI = {
  channel: (channelName) => twitchJsonCall(`/channels/${channelName}`),
  stream:  (channelName) => twitchJsonCall(`/streams/${channelName}`),
  streams: (params = {}) => twitchJsonCall('/streams', params),
  games:   (params = {}) => twitchJsonCall('/games/top', params),
  badges:  (channelName) => twitchJsonCall(`/chat/${channelName}/badges`),

  Beta: {
    badges:        ()          => jsonPCall(`${betaBadgesEndpointPrefix}/global/display`),
    channelBadges: (channelId) => jsonPCall(`${betaBadgesEndpointPrefix}/channels/${channelId}/display`)
  },

  Search: {
    streams:  (params = {}) => twitchJsonCall('/search/streams', params),
    channels: (params = {}) => twitchJsonCall('/search/channels', params),
    games:    (params = {}) => twitchJsonCall('/search/games', params),
  },

  OAuth: {
    authorizeUrl: () => {
      const scopes = [
        'user_follows_edit',
        'user_subscriptions',
        'chat_login',
        'user_read'
      ]

      let url = new URL(OAuthAuthorizeUrl)

      let params = [
        ['response_type', 'code'],
        ['client_id',     TWITCH_CLIENT_ID],
        ['redirect_uri',  TWITCH_APP_REDIRECT_URI],
        ['scope',         scopes.join(' ')]
      ]

      params.forEach(keyValuePair => url.searchParams.append(...keyValuePair))

      return url.href
    },

    token: code => {
      let form = new FormData
      let params = [
        ['client_id',     TWITCH_CLIENT_ID],
        ['client_secret', TWITCH_CLIENT_SECRET],
        ['grant_type',    'authorization_code'],
        ['redirect_uri',  TWITCH_APP_REDIRECT_URI],
        ['code',          code],
      ]

      params.forEach(keyValuePair => form.append(...keyValuePair))

      let init = {
        body: form,
        method: 'POST'
      }

      return jsonCall(OAuthTokenUrl, { init: init })
    }
  },

  followed: (token, params={}) => {
    return twitchJsonCallOauth('/streams/followed', token, params)
  },

  saveToken:       token => Cookies.set(OAuthTokenCookieName, token),
  getToken:        ()    => Cookies.get(OAuthTokenCookieName),
  isAuthenticated: ()    => TwitchAPI.getToken() != undefined,
}

export default TwitchAPI
