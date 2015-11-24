(function() {
  'use strict';

  function TwitchController(ChromecastService, TwitchCastStreamsService) {
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

    vm.streams = [];

    vm.newStream = {
      quality: vm.qualities[0].quality
    };

    vm.apiLoaded = false;
    ChromecastService.onLoad().then(function() {
      vm.apiLoaded = true;
    });


    TwitchCastStreamsService.query (null,
      function(streams) {
        angular.forEach(streams, function(stream) {
          vm.streams.push(stream);
        });
      },
      function(error) {
        console.log(error);
      }
    );

    vm.monitor = function() {
      // Call the service

      // Get the things

      vm.streams.push(angular.copy(vm.newStream));
      // cleaning up for next stream
      vm.newStreamForm.$setPristine();
      vm.newStreamForm.$setUntouched();
      vm.newStream = {
        quality: vm.qualities[0].quality
      };
    };

    vm.discard = function(stream) {
      // remove from display
      vm.streams.splice(vm.streams.indexOf(stream), 1);

      // stuff on the server is done by the component
    };
  }

  angular.module('twitch')
    .controller('TwitchController', TwitchController);
})();
