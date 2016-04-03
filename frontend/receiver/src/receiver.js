var CUSTOM_MESSAGE_BUS_NAME = 'urn:x-cast:twitch.cast.message';

cast.player.api.setLoggerLevel(cast.player.api.LoggerLevel.DEBUG);

var TwitchCastReceiver = function(mediaElement) {
  var self = this;

  var player = null;

  this.on = function(eventType, callback) {
    if (eventType in self.messageTriggers)
      self.messageTriggers[eventType].push(callback);
    else
      self.messageTriggers[eventType] = [callback];
  }

  this.dispatchEvent = function(eventType, message) {
    var callbacks = (self.messageTriggers[eventType] || []);

    for (var callback of callbacks)
      callback(message);
  }

  this.handleCustomMessages = function(event) {
    var message = JSON.parse(event.data);

    self.dispatchEvent(message.type, message);
  }

  this.onLoadEvent = function(event) {
    self.unloadPlayer();

    if (event.data['media'] && event.data['media']['contentId']) {
      var url = event.data['media']['contentId'];
      var host = self.setupHost(url);
      var protocol = cast.player.api.CreateHlsStreamingProtocol(host);

      player = new cast.player.api.Player(host);
      player.load(protocol, 0);
    }
  }

  this.setupHost = function(url) {
    var host = new cast.player.api.Host({
      mediaElement: mediaElement,
      url: url
    });

    host.autoPauseDuration = 3;
    host.autoResumeDuration = 3;
    host.autoResumeNumberOfSegments = 1;
    host.segmentRequestRetryLimit = 10;

    host.onError = self.onHostError;
    host.onAutoPause = self.onHostAutoPause;

    return host;
  }

  this.onHostError = function(errorCode, request) {
    self.unloadPlayer();

    var errorString = 'Fatal Error: ' + errorCode;
    if (request)
      errorString += ' (' + request.errorCode + ', ' + request.status + ')';

    console.error(errorString);
    self.dispatchEvent('notice', { text: errorString });
  }

  this.onHostAutoPause = function(isPaused) {
    var message = {};

    if (isPaused)
      message.text = 'Auto paused';
    else
      message.hide = true;

    self.dispatchEvent('notice', message);
  }

  this.unloadPlayer = function() {
    if (player !== null) {
      player.unload();
      player = null;
    }
  }

  // The mediaManager handles media messages
  var mediaManager = new cast.receiver.MediaManager(mediaElement);
  mediaManager.onLoad = this.onLoadEvent;

  // The castManager allows communication with the sender application
  var castManager = cast.receiver.CastReceiverManager.getInstance();

  // Setting up a custom message bus to communicate with the sender application
  var customMessageBus = castManager.getCastMessageBus(CUSTOM_MESSAGE_BUS_NAME);
  customMessageBus.onMessage = this.handleCustomMessages;

  this.messageTriggers = {};

  castManager.start();
}
