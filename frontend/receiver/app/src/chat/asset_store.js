import TwitchAPI from 'api/twitch.js'
import { jsonCall } from 'api/helpers.js'

const bitBadges = [
  ['bits1',      'https://static-cdn.jtvnw.net/badges/v1/73b5c3fb-24f9-4a82-a852-2f475b59411c/1'],
  ['bits100',    'https://static-cdn.jtvnw.net/badges/v1/09d93036-e7ce-431c-9a9e-7044297133f2/1'],
  ['bits1000',   'https://static-cdn.jtvnw.net/badges/v1/0d85a29e-79ad-4c63-a285-3acd2c66f2ba/1'],
  ['bits5000',   'https://static-cdn.jtvnw.net/badges/v1/57cd97fc-3e9e-4c6d-9d41-60147137234e/1'],
  ['bits10000',  'https://static-cdn.jtvnw.net/badges/v1/68af213b-a771-4124-b6e3-9bb6d98aa732/1'],
  ['bits100000', 'https://static-cdn.jtvnw.net/badges/v1/96f0540f-aa63-49e1-a8b3-259ece3bd098/1']
]

class ChatAssetStore {

  constructor() {
    this.emotes = new Map
    this.badges = new Map(bitBadges)

    this._fetchEmotes()
  }

  loadChannelBadges(channelName) {
    this.badges = new Map(bitBadges)
    let badges = this.badges

    TwitchAPI.badges(channelName)
      .then(data => {
        Object.keys(data).forEach(badgeName => {
          let value = data[badgeName]
          if (value)
            badges.set(badgeName, value.image)
        })
      })

    // Channel specific BTTV emotes
    jsonCall(`//api.betterttv.net/2/channels/${channelName}`)
      .then(this._addBTTVEmotes.bind(this))
  }

  _fetchEmotes() {
    // Global BTTV emotes
    jsonCall('//api.betterttv.net/2/emotes')
      .then(this._addBTTVEmotes.bind(this))
  }

  _addBTTVEmotes(data) {
    let emotes = this.emotes
    let template = data.urlTemplate.replace('{{image}}', '1x')

    data.emotes.forEach(emote => {
      let url = template.replace('{{id}}', emote.id)
      emotes.set(emote.code, url)
    })
  }

}

module.exports = ChatAssetStore
