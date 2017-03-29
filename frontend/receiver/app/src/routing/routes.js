import { object } from 'utils/prelude.js'

const state = object('playing', 'channel', 'quality')

export const routeDescriptors = {
  Home: {
    base: '',
    tagNames: ['home-view'],
    getState: (..._) => state(false)
  },

  Loading: {
    base: '',
    tagNames: [undefined, 'loading-view', 'loading-view'],
    getState: (..._) => state(false)
  },

  Watch: {
    base: 'watch',
    tagNames: [undefined, undefined, 'stream-view'],
    getState: (...args) => state(true, ...args)
  }
}

const routeLink = (descriptor, ...path) =>
  [descriptor.base, ...path].map(encodeURIComponent).join('/')

const r = routeDescriptors

export const routeLinks = {
  home:   ()                    => routeLink(r.Home                        ),
  load:   (channel, ...quality) => routeLink(r.Loading, channel, ...quality),
  watch:  (channel, quality)    => routeLink(r.Watch,   channel, quality   ),
}

