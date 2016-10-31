import { loadScript } from 'utils/deferred_load.js'

function jsonCall(url, init={}) {
  let promise =
    fetch(url, init)
      .then(response => {
        if (response.ok)
          return response.json()
        else
          throw new Error(response.status)
      })

  return promise
}

// Naive way of doing things
function jsonPCall(url) {
  return new Promise((resolve, _) => {
    window.callback = resolve

    return loadScript(`${url}?callback=callback`)
  })
}

module.exports = {
  jsonCall: jsonCall,
  jsonPCall: jsonPCall,
}
