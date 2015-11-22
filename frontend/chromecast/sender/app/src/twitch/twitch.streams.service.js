(function() {
  'use strict';

  angular.module('twitch')
    .factory('TwitchCastStreamsService', function($resource) {
      return $resource('/streamer/streams/:streamId', {streamId: '@id'}, {
        // query
        // get
        // post
        // delete
      });
    });
})();
