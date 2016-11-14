import { loadScript } from 'utils/deferred_load.js'

export function jsonCall(rawUrl, { init={}, params={} } = {}) {
  if (rawUrl.startsWith('//'))
    rawUrl = `${window.location.protocol}${rawUrl}`

  let url = new URL(rawUrl)

  Object.keys(params).forEach(key => {
    url.searchParams.append(key, params[key])
  })

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
export function jsonPCall(url) {
  return new Promise((resolve, _) => {
    window.callback = resolve

    return loadScript(`${url}?callback=callback`)
  })
}
