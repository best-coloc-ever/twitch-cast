import { routes } from './routes.js'

import ChromecastMessage, { ChromecastMessageType } from 'chromecast/messages.js'

const dummyView = { unmount: (..._) => undefined }

export default class Router {

  constructor(domNode, opts) {
    this.mountNode = domNode
    this.opts = opts

    this.currentView = dummyView
    this.routeState = null

    routes.forEach(descriptor => this.addRoute(...descriptor))

    opts.receiver.on(ChromecastMessageType.ReceiverState, () => {
      let message = ChromecastMessage.receiverStateResponse(this.routeState)

      opts.receiver.sendCustomMessage(message)
    })
  }

  addRoute(path, tagName, state) {
    riot.route(path, (...args) => {
      this.setView(tagName, ...args)
      this.routeState = state(...args)
    })
  }

  setView(tagName, ...args) {
    this.currentView.unmount(true)

    let context = Object.assign(this.opts, { routeOpts: args })
    let children = riot.mount(this.mountNode, tagName, context)

    if (children.length) {
      this.currentView = children[0]
      this.currentView.update()
    }
    else {
      console.error(`Missing tag: "${tagName}"`)
      this.currentView = dummyView
    }
  }

  start() {
    riot.route.start(true)
  }

}
