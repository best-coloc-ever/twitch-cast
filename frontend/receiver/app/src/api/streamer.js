function path(suffix) {
  return `/streamer${suffix}`
}

function jsonCall(route, extra={}) {
  let init = Object.assign(extra, {})

  let promise =
    fetch(path(route), init)
      .then(response => {
        if (response.ok)
          return response.json()
        else
          throw new Error(response.status)
      })

  return promise
}

function getStream(streamId) {
  return jsonCall(`/streams/${streamId}`)
}

const StreamerAPI = {
  stream: getStream
}

module.exports = StreamerAPI
