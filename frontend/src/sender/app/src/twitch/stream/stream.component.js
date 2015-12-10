(function() {
  'use strict';

  angular.module('twitch')
    .component('bceStream', {
      bindings: {
        stream: '=',
        discard: '&onDiscard'
      },
      controller: function(TwitchCastStreamsService, ChromecastService, $timeout) {
        var vm = this;

        if (!vm.stream.id) {
          TwitchCastStreamsService.save(null, {
            channel: vm.stream.channel,
            quality: vm.stream.quality
          }, function(stream) {
            vm.updateStream(stream);
          }, function(error) {
            vm.unmonitor(vm.stream);
          });
        }

        vm.doStuff = function() {
          console.log('stuff is done');
        };

        vm.unmonitor = function() {
          // remove from display
          vm.discard(vm.stream);
          // do stuff on the server
          if (vm.stream.id)
            vm.stream.$delete();
        };

        vm.watch = function() {
          vm.working = true;
          vm.stream.$watch(
            function(stream) {
              vm.working = false;
              vm.updateStream(stream);
              // TODO: use websockets instead of polling
              $timeout(vm.poll, 10000);
            },
            function(error) {
              vm.working = false;
            });
        };

        vm.stopWatch = function() {
          vm.stream.$unwatch(function(stream) {
            vm.updateStream(stream);
          }, function(error) {
            console.error(error);
          });
        };

        vm.cast = function() {
          ChromecastService.cast(vm.stream.proxy);
        };

        vm.poll = function() {
          vm.stream.$get(function(stream) {
            vm.updateStream(stream);
            if (vm.stream.proxy && !vm.stream.proxy.ready)
              $timeout(vm.poll, 2000);
          });
        }

        // Only way I found to noy mess up the stream collection of the
        // TwitchController
        vm.updateStream = function(newStream) {
          for (var key in newStream)
            vm.stream[key] = newStream[key];
        }
      },
      template: function($templateCache) {
        return $templateCache.get(
          'src/twitch/stream/stream.component.html');
      }
    });
})();
