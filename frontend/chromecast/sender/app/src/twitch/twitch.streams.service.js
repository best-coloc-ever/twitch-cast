(function() {
  'use strict';

  angular.module('twitch')
    .factory('TwitchCastStreamsService', function($resource) {
      return $resource('/streamer/streams/:id/:action', {id: '@id'}, {
        // query
        // get
        // post
        // delete
        watch: {
          method: 'POST',
          params: {
            action: 'watch'
          }
        },
        unwatch: {
          method: 'POST',
          params: {
            action: 'unwatch'
          }
        },
      });
    });
})();
