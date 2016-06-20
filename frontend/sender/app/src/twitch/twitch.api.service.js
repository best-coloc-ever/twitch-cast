(function() {
  'use strict';

  angular.module('twitch')
    .factory('TwitchAPIService', function($http) {
      var CLIENT_ID = '5sdczsrlv5d4mnx9j8ejrargx34oev4';
      var BASE_URL = 'https://api.twitch.tv/kraken/';

      var config = {
        headers: {
          'Client-ID': CLIENT_ID
        }
      };

      return {
        channel: function(channelName, callback) {
          $http.get(
            BASE_URL + 'channels/' + channelName,
            config
          ).then(function(response) {
            callback(response.data)
          });
        },
        stream: function(channelName, callback) {
          $http.get(
            BASE_URL + 'streams/' + channelName,
            config
          ).then(function(response) {
            callback(response.data.stream);
          });
        }
      }
    })
})();
