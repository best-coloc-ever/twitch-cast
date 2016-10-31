const cheerColorsByMilestone = [
  [100000, 'gold'], [10000, 'red'], [5000, 'blue'],
  [1000, 'green'], [100, 'purple'], [1, 'gray']
]

const cheerTheme = 'dark',
      cheerType  = 'animated',
      cheerSize  = '1'

function cheerColor(cheerCount) {
  for (let [milestone, color] of cheerColorsByMilestone) {
    if (cheerCount >= milestone)
      return color
  }
  // Should not happen
  return 'unknown'
}

function cheerEmote(cheerCount) {
  let color = cheerColor(cheerCount)

  return `https://static-cdn.jtvnw.net/bits/${cheerTheme}/${cheerType}/${color}/${cheerSize}`
}

module.exports = {
  cheerColor: cheerColor,
  cheerEmote: cheerEmote
}
