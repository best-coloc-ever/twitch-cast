(function() {
  'use strict';

  angular.module('twitch')
    .factory('TwitchCastProxiesService', function($resource) {
      return $resource('/hls-proxy/proxies/:proxyId', {proxyId: '@id'}, {
        // query
        // get
        // post
        // delete
      });
    });
})();
