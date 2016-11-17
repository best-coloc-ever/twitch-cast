import { routes } from './routes.js'

const dummyView = { unmount: (..._) => undefined }

export default class Router {

  constructor(domNode, opts) {
    this.mountNode = domNode
    this.opts = opts
    this.currentView = dummyView

    routes.forEach(descriptor => this.addRoute(...descriptor))
  }

  addRoute(path, tagName) {
    riot.route(path, (...args) => {
      this.setView(tagName, ...args)
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
