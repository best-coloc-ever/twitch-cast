(function() {
  'use strict';

  angular
    .module('TwitchCaster', ['ngMaterial', 'twitch'])
    .config(function($mdThemingProvider) {
      $mdThemingProvider.theme('default')
        .primaryPalette('deep-purple');
        // .accentPalette('red');

    });
})();
