(function() {
  'use strict';

  function ChromecastService($window, CHROMECAST_APP_ID, $q, $timeout) {
    var session = null;
    var currentProxy = null;
    var loadSuccessful = false;
    var deferred;

    function getLoaded() {
      deferred = $q.defer();

      if (loadSuccessful) {
        deferred.resolve();
      } else {
        $timeout(function() {
          deferred.reject('Taking too long loading Chromecast API');
        }, 5000);
      }

      return deferred.promise;
    }

    function sessionListener(e) {
      session = e;
      if (session.media.length)
        console.log('Session discovered: ' + session.media[0]);
    }

    function play() {
      var mediaInfo = new chrome.cast.media.MediaInfo(
        currentProxy.url,
        'application/vnd.apple.mpegurl'
      );
      var request = new chrome.cast.media.LoadRequest(mediaInfo);

      function onMediaDiscovered(how, media) {
        currentMedia = media;
      }

      session.loadMedia(
        request,
        onMediaDiscovered.bind(this, 'loadMedia'),
        console.error.bind(console, 'Error loading media')
      );
    }

    function onRequestSessionSuccess(e) {
      console.log('Session success:');
      session = e;

      play();
    }

    function cast() {
      if (currentProxy !== null) {
        if (session === null)
          chrome.cast.requestSession(
            onRequestSessionSuccess,
            console.error.bind(console, 'Failed to launch a session request')
          );
        else
          play();
      } else
        alert('No current stream!');
    }

    function receiverListener(e) {
      if (e === chrome.cast.ReceiverAvailability.AVAILABLE) {
        console.log('Receiver available');
      }
    }

    function initializeCastApi() {
      var sessionRequest = new chrome.cast.SessionRequest(CHROMECAST_APP_ID);
      var apiConfig = new chrome.cast.ApiConfig(
        sessionRequest,
        sessionListener,
        receiverListener
      );

      chrome.cast.initialize(
        apiConfig,
        console.log.bind(console, 'Chromecast succesfully initialized'),
        console.error.bind(console, 'Error while initializing Chromecast')
      );
      loadSuccessful = true;
      if (deferred) {
        deferred.resolve();
      }
    }

    function initialize(loaded, errorInfo) {
      if (loaded)
        initializeCastApi();
      else
        console.log(errorInfo);
    }

    function setProxy(proxy) {
      currentProxy = proxy;
    }

    $window.__onGCastApiAvailable = initialize;

    return {
      setProxy: setProxy,
      onLoad: getLoaded
    };
  }

  angular.module('chromecast')
    .factory('ChromecastService', ChromecastService);
})();
