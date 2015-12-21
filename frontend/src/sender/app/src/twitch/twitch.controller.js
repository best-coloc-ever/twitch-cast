(function() {
  'use strict';

  function TwitchController(ChromecastService, TwitchCastStreamsService, TwitchCastWebsocketService) {
    var vm = this;

    vm.qualities = [{
      name: 'Source (best)',
      quality: 'best'
    }, {
      name: 'High',
      quality: 'high'
    }, {
      name: 'Medium',
      quality: 'medium'
    }, {
      name: 'Low',
      quality: 'low'
    }, {
      name: 'Worst',
      quality: 'worst'
    }];

    vm.streams = {};

    vm.newStream = {
      quality: vm.qualities[0].quality
    };

    vm.apiLoaded = false;
    ChromecastService.onLoad().then(function() {
      vm.apiLoaded = true;
    });


    TwitchCastStreamsService.query(null,
      function(streams) {
        angular.forEach(streams, vm.addStream);
      },
      function(error) {
        console.error(error);
      }
    );

    vm.monitor = function() {
      vm.addStream(new TwitchCastStreamsService(angular.copy(vm.newStream)));
      // cleaning up for next stream
      vm.newStreamForm.$setPristine();
      vm.newStreamForm.$setUntouched();
      vm.newStream = {
        quality: vm.qualities[0].quality
      };
    };

    vm.discard = function(stream) {
      // remove from display
      var key = [stream.channel, stream.quality];
      delete vm.streams[key];
    };

    vm.addStream = function(stream) {
      var key = [stream.channel, stream.quality];
      if (!(key in vm.streams))
        vm.streams[key] = stream;
    }

    TwitchCastWebsocketService.on('monitored', function(streamData) {
      vm.addStream(new TwitchCastStreamsService(streamData));
    });

    TwitchCastWebsocketService.on('unmonitored', vm.discard);
  }

  angular.module('twitch')
    .controller('TwitchController', TwitchController);
})();
