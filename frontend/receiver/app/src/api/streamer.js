import { jsonCall } from './helpers.js'

function streamerJsonCall(route) {
  let path = `/streamer${route}`

  return jsonCall(path)
}

const StreamerAPI = {
  stream: (streamId) => streamerJsonCall(`/streams/${streamId}`)
}

module.exports = StreamerAPI
