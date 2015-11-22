(function() {
  'use strict';

  angular.module('twitch')
    .component('bceStream', {
      bindings: {
        stream: '=',
        discard: '&onDiscard'
      },
      controller: function(TwitchCastStreamsService, TwitchCastProxiesService) {
        var vm = this;

        vm.doStuff = function () {
          console.log('stuff is done');
        };

        vm.unmonitor = function(stream) {
          // remove from display
          vm.discard(vm.stream);
          // do stuff on the server
        };

        vm.watch = function (stream) {
          vm.stream.port = 69;
          vm.working = true;
        };

        vm.stopWatch = function (stream) {
          delete vm.stream.port;
          vm.working = false;
        };
      },
      templateUrl: '/src/twitch/stream/stream.component.html'
    });
})();
