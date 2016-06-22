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
        },
        streams: function(query, callback) {
          var params = {
            params: {
              query: query,
              limit: 50
            }
          };
          angular.merge(params, config);
          console.log(params);

          $http.get(
            BASE_URL + 'search/streams',
            params
          ).then(function(response) {
            callback(response.data.streams);
          })
        }
      }
    })
})();
