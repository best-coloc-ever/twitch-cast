(function() {
  'use strict';

  angular.module('twitch')
    .component('bceStream', {
      bindings: {
        stream: '=',
        discard: '&onDiscard'
      },
      controller: function(
        TwitchCastStreamsService,
        ChromecastService,
        TwitchCastWebsocketService,
        TwitchAPIService,
        $interval
      ) {
        var vm = this;

        var REFRESH_INFO_INTERVAL = 30;

        // Initialize state
        vm.present = (vm.stream.id != null);
        vm.watchable = (vm.present && !vm.stream.proxy);
        vm.castable = (vm.stream.proxy && vm.stream.proxy.ready);
        vm.readying = (vm.stream.proxy && !vm.stream.proxy.ready);
        vm.viewers = '?';
        vm.views = '?';
        vm.follows = '?';

        function fetchInfos() {
          TwitchAPIService.channel(vm.stream.channel, function(channel) {
            vm.thumbnail = channel.logo;
            vm.views = channel.views.toLocaleString();
            vm.follows = channel.followers.toLocaleString();
            vm.game = channel.game;
          })

          TwitchAPIService.stream(vm.stream.channel, function(stream) {
            if (stream) {
              vm.viewers = stream.viewers.toLocaleString();
              vm.preview = stream.preview.large;
              vm.live = true;
            }
            else
              vm.live = false;
          });
        }

        $interval(fetchInfos, REFRESH_INFO_INTERVAL * 1000);
        fetchInfos();

        if (!vm.present) {
          TwitchCastStreamsService.save(null, {
            channel: vm.stream.channel,
            quality: vm.stream.quality
          }, function(stream) {
            console.log(stream);
            vm.updateStream(stream);
            vm.present = true;
            vm.watchable = true;
          }, function(error) {
            vm.unmonitor(vm.stream);
          });
        }

        vm.unmonitor = function() {
          // remove from display
          vm.discard(vm.stream);
          // do stuff on the server
          if (vm.present)
            vm.stream.$delete();
        };

        vm.watch = function() {
          vm.watchable = false;
          vm.readying = true;

          vm.stream.$watch(
            function(stream) {
              vm.updateStream(stream);
              fetchInfos();
              // Castablity will be set thanks to the websocket
            },
            function(error) {
              console.error(error);
              vm.watchable = true;
              vm.readying = false;
            });
        };

        vm.unwatch = function() {
          vm.stream.$unwatch(function(stream) {
            vm.updateStream(stream);
            vm.watchable = true;
            vm.readying = false;
            vm.castable = false;
          }, function(error) {
            console.error(error);
          });
        };

        vm.cast = function() {
          ChromecastService.setChannel(vm.stream.channel);
          ChromecastService.cast(vm.stream.proxy);
        };

        vm.toggleChat = function() {
          ChromecastService.toggleChat();
        }

        vm.positionChat = function() {
          ChromecastService.positionChat();
        }

        // Only way I found to not mess up the stream collection of the
        // TwitchController
        vm.updateStream = function(newStream) {
          for (var key in newStream)
            vm.stream[key] = newStream[key];
        }

        TwitchCastWebsocketService.on('watched', function(streamData) {
          if (streamData.id == vm.stream.id) {
            vm.stream.proxy = streamData.proxy;
            vm.watchable = false;
            vm.readying = true;
          }
        });

        TwitchCastWebsocketService.on('unwatched', function(streamData) {
          if (streamData.id == vm.stream.id) {
            vm.stream.proxy = null;
            vm.watchable = true;
            vm.readying = false;
            vm.castable = false;
          }
        });

        TwitchCastWebsocketService.on('ready', function(streamData) {
          if (streamData.id == vm.stream.id) {
            vm.stream.proxy = streamData.proxy;
            vm.watchable = false;
            vm.readying = false;
            vm.castable = true;
          }
        });

      },
      template: function($templateCache) {
        return $templateCache.get(
          'src/twitch/stream/stream.component.html');
      }
    });
})();
