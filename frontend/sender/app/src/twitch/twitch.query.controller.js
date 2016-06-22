(function() {
  'use strict';

  angular
    .module('twitch')
    .controller('TwitchQueryController', TwitchQueryController);

  function TwitchQueryController(TwitchAPIService, $log, $q) {
    var self = this;

    self.querySearch = querySearch;

    function querySearch(query) {
      $log.info('query search');

      var deferred = $q.defer();
      TwitchAPIService.streams(query, function(streams) {
        var results = streams.sort(function(s1, s2) {
          return s2.viewers - s1.viewers;
        });
        deferred.resolve(results);
      })
      return deferred.promise;
    }

  }
})();
