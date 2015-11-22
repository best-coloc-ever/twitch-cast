(function() {
  'use strict';

  function TwitchController(ChromecastService, TwitchCastStreamsService, TwitchCastProxiesService) {
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

    vm.streams = [{
      name: 'day9tv',
      quality: 'best'
    }];

    vm.newStream = {};

    vm.apiLoaded = false;
    ChromecastService.onLoad().then(function() {
      vm.apiLoaded = true;
    });

    vm.monitor = function() {
      // Call the service

      // Get the things

      vm.streams.push(angular.copy(vm.newStream));
      // cleaning up for next stream
      // for (var prop in vm.newStream) {
      //   if (obj.hasOwnProperty(prop)) {
      //     delete obj[prop];
      //   }
      // }
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
