import TwitchAPI from 'api/twitch.js'
import { jsonCall } from 'api/helpers.js'

export default class ChatAssetStore {

  constructor(channel) {
    this.channel = channel

    this.emotes = new Map
    this.badges = new Map

    this._fetchBadges()
    this._fetchEmotes()
  }

  _fetchEmotes() {
    // Global BTTV emotes
    jsonCall('//api.betterttv.net/2/emotes')
      .then(this._addBTTVEmotes.bind(this))

    // Channel specific BTTV emotes
    jsonCall(`//api.betterttv.net/2/channels/${this.channel}`)
      .then(this._addBTTVEmotes.bind(this))
  }

  _fetchBadges() {
    TwitchAPI.Beta.badges()
      .then(data => {
        this._addBadges(data)
        this._fetchChannelBadges()
      })
  }

  _fetchChannelBadges() {
    TwitchAPI.channel(this.channel)
      .then(data => {
        TwitchAPI.Beta.channelBadges(data._id)
          .then(this._addBadges.bind(this))
      })
  }

  _addBTTVEmotes(data) {
    let emotes = this.emotes
    let template = data.urlTemplate.replace('{{image}}', '1x')

    data.emotes.forEach(emote => {
      let url = template.replace('{{id}}', emote.id)
      emotes.set(emote.code, url)
    })
  }

  _addBadges(data) {
    let badges = this.badges

    Object.keys(data.badge_sets).forEach(badgePrefix => {
      let versions = data.badge_sets[badgePrefix].versions
      Object.keys(versions).forEach(version => {
        let badgeUrl = versions[version].image_url_1x
        badges.set(`${badgePrefix}/${version}`, badgeUrl)
      })
    })
  }

}
