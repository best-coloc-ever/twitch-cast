import { jsonCall } from './helpers.js'

function streamerPath(route) {
  return `//streamer.${window.location.host}${route}`
}

function streamerJsonCall(route) {
  let path = streamerPath(route)

  return jsonCall(path)
}

const StreamerAPI = {
  stream:      (channel)          => streamerJsonCall(`/${channel}`),
  receiverUrl: (channel)          => `/chromecast#${channel}`,
  playlistUrl: (channel, quality) => {
    // Video data needs lowercased access
    let lcChannel = channel.toLowerCase()
    let lcQuality = quality.toLowerCase()

    return streamerPath(`/${lcChannel}/${lcQuality}/index.m3u8`)
  }
}

export default StreamerAPI
