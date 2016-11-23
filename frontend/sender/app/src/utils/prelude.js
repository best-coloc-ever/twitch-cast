export function zip(a1, a2) {
  return a1.map((e, i) => [e, a2[i]])
}

function addProperty(obj, [k, v]) {
  obj[k] = v
  return obj
}

export function object(...properties) {
  return (...values) => {
    return zip(properties, values).reduce(addProperty, new Object)
  }
}
