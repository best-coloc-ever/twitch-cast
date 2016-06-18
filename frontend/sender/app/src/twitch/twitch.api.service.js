(function() {
  'use strict';

  angular.module('twitch')
    .factory('TwitchAPIService', function($http) {
      return {
        channel: function(channelName, callback) {
          $http.get('https://api.twitch.tv/kraken/channels/' + channelName).then(callback)
        },
        stream: function(channelName, callback) {
          $http.get('https://api.twitch.tv/kraken/streams/' + channelName).then(callback)
        }
      }
    })
})();
