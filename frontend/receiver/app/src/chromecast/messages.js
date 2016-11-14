export const chromecastCustomMessageBus = 'urn:x-cast:twitch.cast.message'

function message(type, data) {
  return {
    type: type,
    data: data
  }
}

export const ChromecastMessageType = {
  Watch: 'watch'
}

const ChromecastMessage = {
  watch: channel => message(ChromecastMessageType.Watch, { channel: channel })
}

export default ChromecastMessage
