export function jsonCall(url, init={}) {
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
