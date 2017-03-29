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

export const ChatFlavors = {
  Classic: 'classic',
  TwoPart: 'two part',
}

export const ChromecastMessageType = {
  ReceiverState: 'reciever-state',
  ToggleFullscreen: 'toggle-fullscreen',
  ChatPosition: 'chat-position',
  ChatSize: 'chat-size',
  ChatFlavor: 'chat-flavor',
}

const ChromecastMessage = {

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

  chatSize: size => {
    let data = { size: size }

    return message(ChromecastMessageType.ChatSize, data)
  },

  chatFlavor: flavor => {
    let data = { flavor: flavor }

    return message(ChromecastMessageType.ChatFlavor, data)
  },

}

export default ChromecastMessage
