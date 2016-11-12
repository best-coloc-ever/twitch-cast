import { routes } from "./routes.js"

const dummyView = { unmount: (..._) => undefined }

export class Router {

  constructor(domNode) {
    this.mountNode = domNode
    this.currentView = dummyView

    routes.forEach(descriptor => this.addRoute(...descriptor))
  }

  addRoute(path, tagName) {
    riot.route(path, (...args) => {
      this.setView(tagName)
    })
  }

  setView(tagName) {
    this.currentView.unmount(true)

    let children = riot.mount(this.mountNode, tagName)

    if (children.length)
      this.currentView = children[0]
    else {
      console.error(`Missing tag: "${tagName}"`)
      this.currentView = dummyView
    }
  }

  start() {
    riot.route.start(true)
  }

}
