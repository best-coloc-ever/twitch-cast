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
    vm.streamProperties = {};

    vm.newStream = {
      quality: vm.qualities[0].quality
    };

    vm.apiLoaded = false;
    ChromecastService.onLoad().then(function() {
      vm.apiLoaded = true;
    });

    function streamHash(stream) {
      return [stream.channel, stream.quality];
    }

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
      delete vm.streams[streamHash(stream)];
    };

    vm.addStream = function(stream) {
      var key = streamHash(stream);
      if (!(key in vm.streams))
        vm.streams[key] = stream;
    }

    vm.updateStream = function(newStream) {
      vm.streams[streamHash(newStream)] = newStream;
    }

    vm.streamPropertyUpdate = function(stream, property, value) {
      var key = streamHash(stream);

      if (!(key in vm.streamProperties))
        vm.streamProperties[key] = {};

      vm.streamProperties[key][property] = value;
    }

    function streamWeight(stream) {
      var properties = vm.streamProperties[streamHash(stream)];

      if (properties)
        return (
          (properties.live || 0) +
          (properties.castable || 0) +
          Math.min((properties.viewers || 0) / 1000000, 1)
        );

      return 0;
    }

    vm.sortedStreams = function() {
      var streams = Object.keys(vm.streams).map(function(k) {
        return vm.streams[k];
      });

      return streams.sort(function(s1, s2) {
        var s1Weight = streamWeight(s1),
            s2Weight = streamWeight(s2);

        return s2Weight - s1Weight;
        // return isStreamLive(s1) - isStreamLive(s2);
      })
    }

    TwitchCastWebsocketService.on('monitored', function(streamData) {
      vm.addStream(new TwitchCastStreamsService(streamData));
    });

    TwitchCastWebsocketService.on('unmonitored', vm.discard);
  }

  angular.module('twitch')
    .controller('TwitchController', TwitchController);
})();
