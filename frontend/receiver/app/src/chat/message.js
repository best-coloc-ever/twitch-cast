import { cheerColor, cheerEmote } from './cheers.js'

const defaultColors = [
  '#FF0000', '#0000FF', '#00FF00', '#B22222', '#FF7F50',
  '#9ACD32', '#FF4500', '#2E8B57', '#DAA520', '#D2691E',
  '#5F9EA0', '#1E90FF', '#FF69B4', '#8A2BE2', '#00FF7F'
]

const cheerPattern = /^cheer(\d+)$/

// https://discuss.dev.twitch.tv/t/default-user-color-in-chat/385
function defaultColor(name) {
  let n = name.charCodeAt(0) + name.charCodeAt(name.length - 1)

  return defaultColors[n % defaultColors.length]
}

function getBadges(message) {
  if (message.tags && message.tags.badges)
    return message.tags.badges.split(',')
  else
    return []
}

function emoteUrl(emoteId) {
  return `//static-cdn.jtvnw.net/emoticons/v1/${emoteId}/1.0`
}

export function buildChatLine(message, store) {
  let li = $('<li>')
    .addClass('chat-line')

  let badges = getBadges(message)

  if (badges.length) {
    let badgeSpan = $('<span>')
      .addClass('badge-wrapper')

    let div = $('<div>')
      .addClass('badge-container')

    badges.forEach(badge => {
      let img = $('<img>')
        .attr('src', store.badges.get(badge))
        .addClass('badge')

      div.append(img)
    })

    badgeSpan.append(div)
    li.append(badgeSpan)
  }

  let mTags = (message.tags || {})
  let color = (mTags.color || defaultColor(message.sender))

  let nameSpan = $('<span>')
    .addClass('chat-line-sender')
    .text(mTags['display-name'] || message.sender)
    .css('color', color)

  let emotePositions = []
  if (mTags.emotes) {
    mTags.emotes.split('/').forEach(emoteDescriptor => {
      let [emoteId, positions] = emoteDescriptor.split(':')
      positions.split(',').forEach(position => {
        let [startIdx, endIdx] = position.split('-')
        emotePositions.push([parseInt(startIdx), parseInt(endIdx), emoteId])
      })
    })
  }
  emotePositions.sort((a, b) => a[0] - b[0])

  let getNextEmoteIdx = () => (emotePositions.length ? emotePositions[0][0] : -1)
  let makePart = (word) => {
    if (store.emotes.has(word))
      return `<img src="${store.emotes.get(word)}">`

    if (mTags.bits) {
      let match = word.match(cheerPattern)
      if (match)
        return `<img src="${cheerEmote(match[1])}">` +
               `<span style="color: ${cheerColor(match[1])};">${match[1]}</span>`
    }

    return word
  }

  let htmlParts = []
  let currentWord = ''
  let nextEmoteIdx = getNextEmoteIdx()
  for (let i = 0; i < message.content.length; ++i) {
    if (i == nextEmoteIdx) {
      let [_, emoteEndIdx, emoteId] = emotePositions[0]
      htmlParts.push(`<img src="${emoteUrl(emoteId)}">`)
      i = emoteEndIdx
      emotePositions.splice(0, 1)
      nextEmoteIdx = getNextEmoteIdx()
    }
    else {
      let chr = message.content[i]
      if (chr == ' ') {
        htmlParts.push(makePart(currentWord))
        currentWord = ''
      }
      else
        currentWord += chr
    }
  }
  // Final word
  if (currentWord)
    htmlParts.push(makePart(currentWord))

  let contentSpan = $('<span>').html(`: ${htmlParts.join(' ')}`)

  li.append(nameSpan)
  li.append(contentSpan)

  return li
}
