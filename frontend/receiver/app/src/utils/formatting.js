export function zpad(what, n) {
  what = what + '' // Make it a string

  if (what.length >= n)
    return what

  return new Array(n - what.length + 1).join('0') + what
}
