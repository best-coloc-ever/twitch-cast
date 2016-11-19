export const chromecastCustomMessageBus = 'urn:x-cast:twitch.cast.message'

function message(type, data={}) {
  return {
    type: type,
    data: data
  }
}

export const ChatPositions = {
  Left: 'left',
  Right: 'right',
}

export const ChromecastMessageType = {
  Watch: 'watch',
  ReceiverState: 'reciever-state',
  ToggleFullscreen: 'toggle-fullscreen',
  ChatPosition: 'chat-position',
}

const ChromecastMessage = {

  watch: (channel, quality = null) => {
    let data = { channel: channel }

    if (quality)
      data.quality = quality

    return message(ChromecastMessageType.Watch, data)
  },

  toggleFullscreen: enabled => {
    let data = { enabled: enabled }

    return message(ChromecastMessageType.ToggleFullscreen, data)
  },

  receiverStateRequest: () => message(ChromecastMessageType.ReceiverState),
  receiverStateResponse: state => message(ChromecastMessageType.ReceiverState, state),

  chatPosition: position => {
    let data = { position: position }

    return message(ChromecastMessageType.ChatPosition, data)
  },

}

export default ChromecastMessage
