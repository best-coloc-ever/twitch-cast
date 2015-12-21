(function() {
  'use strict';

  angular.module('twitch')
    .factory('TwitchCastWebsocketService', function($websocket, $location) {
      var dataStream = $websocket('ws://' + $location.host() + '/streamer/events');

      var triggers = {};

      dataStream.onMessage(function(message) {
        var data = JSON.parse(message.data);
        // console.log(data.event);

        if (data.event in triggers) {
          var callbacks = triggers[data.event];
          callbacks.forEach(function(callback) {
            callback(data.stream);
          });
        }
      });

      return {
        on: function(event, callback) {
          if (event in triggers)
            triggers[event].push(callback);
          else
            triggers[event] = [callback];
        }
      };
    });
})();
