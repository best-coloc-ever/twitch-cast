function gcd(a, b) {

  while (b > 0) {
    let tmp = b
    b = a % b
    a = tmp
  }

  return a
}

function lcm(a, b) {
  return a * (b / gcd(a, b))
}

export function greatestCommonDivisor(a1, ...aN) {
  return aN.reduce(gcd, a1)
}

export function leastCommonMultiple(a1, ...aN) {
  return aN.reduce(lcm, a1)
}
