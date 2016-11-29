import { routeDescriptors } from './routes.js'

export const RouterEvent = {
  RouteChanged: 'router-route-changed',
}

const dummyView = { unmount: (..._) => undefined }

export default class Router {

  constructor(domNode) {
    this.mountNode = domNode
    this.currentView = dummyView

    riot.observable(this)

    Object.keys(routeDescriptors).forEach(k =>
      this.addRoute(routeDescriptors[k])
    )
  }

  addRoute(routeDescriptor) {
    let pattern = `/${routeDescriptor.base}`

    routeDescriptor.tagNames.forEach(tagName => {
      if (tagName)
        riot.route(pattern, (...path) => {
          let decodedPath = path.map(decodeURIComponent)

          this.trigger(RouterEvent.RouteChanged, routeDescriptor, decodedPath)
          this.setView(tagName, decodedPath)
        })

      pattern = (pattern == '/' ? '/*' : `${pattern}/*`)
    })
  }

  setView(tagName, path) {
    this.currentView.unmount(true)

    let children = riot.mount(
      this.mountNode,
      tagName,
      { path: path }
    )

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
