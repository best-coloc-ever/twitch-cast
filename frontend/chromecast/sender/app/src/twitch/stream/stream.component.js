(function() {
  'use strict';

  angular.module('twitch')
    .component('bceStream', {
      bindings: {
        stream: '=',
        discard: '&onDiscard'
      },
      controller: function(TwitchCastStreamsService, ChromecastService) {
        var vm = this;

        if (!vm.stream.id) {
          TwitchCastStreamsService.save(null, {
            channel: vm.stream.channel,
            quality: vm.stream.quality
          }, function(stream) {
            vm.stream = stream;
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
          vm.stream.$delete();
        };

        vm.watch = function() {
          vm.working = true;
          vm.stream.$watch(
            function(stream) {
              vm.working = false;
              vm.stream = stream;
            },
            function(error) {
              vm.working = false;
            });
        };

        vm.stopWatch = function() {
          vm.stream.$unwatch(function(stream) {
            vm.stream = stream;
          }, function(error) {
            console.error(error);
          });
        };

        vm.cast = function() {
          ChromecastService.cast(vm.stream.proxy);
        };

      },
      template: function($templateCache) {
        return $templateCache.get(
          'src/twitch/stream/stream.component.html');
      }
    });
})();
