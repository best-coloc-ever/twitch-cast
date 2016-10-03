import { jsonCall } from './helpers.js'

function streamerJsonCall(route) {
  let path = `/streamer${route}`

  return jsonCall(path)
}

function getStream(streamId) {
  return streamerJsonCall(`/streams/${streamId}`)
}

const StreamerAPI = {
  stream: getStream
}

module.exports = StreamerAPI
