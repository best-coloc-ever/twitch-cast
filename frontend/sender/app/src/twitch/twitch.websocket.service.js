(function() {
  'use strict';

  angular.module('twitch')
    .factory('TwitchCastWebsocketService', function($websocket, $location, $interval) {
      var dataStream = $websocket('ws://' + $location.host() + '/streamer/events');

      var triggers = {};

      dataStream.onMessage(function(message) {
        var data = JSON.parse(message.data);

        if (data.event in triggers) {
          var callbacks = triggers[data.event];
          callbacks.forEach(function(callback) {
            callback(data.stream);
          });
        }
      });

      dataStream.onOpen(function() {
        $interval(function() {
          dataStream.send('ping');
        }, 30000);
      });

      dataStream.onClose(function() {
        console.log('closed');
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
