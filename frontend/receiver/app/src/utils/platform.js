export function isChromecastDevice() {
  return (navigator.userAgent.indexOf('CrKey') != -1)
}
