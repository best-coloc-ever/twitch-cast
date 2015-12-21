var player = null;

var notice = $('#debug-notice');
var mediaElement = $('#video-player').get(0);

function initalizeReceiver() {
  cast.player.api.setLoggerLevel(cast.player.api.LoggerLevel.DEBUG)

  // The mediaManager handles media messages
  var mediaManager = new cast.receiver.MediaManager(mediaElement);
  mediaManager.onLoad = onLoadEvent;

  // The castReceiverManager allows communication with the sender application
  var castReceiverManager = cast.receiver.CastReceiverManager.getInstance();
  castReceiverManager.start();
}

function onLoadEvent(event) {
  unloadPlayer();

  if (event.data['media'] && event.data['media']['contentId']) {
    var url = event.data['media']['contentId'];
    var host = setupHost(url);
    var protocol = cast.player.api.CreateHlsStreamingProtocol(host);

    player = new cast.player.api.Player(host);
    player.load(protocol, 0);

    notice.hide();
  }
}

function setupHost(url) {
  var host = new cast.player.api.Host({
    mediaElement: mediaElement,
    url: url
  });

  host.autoPauseDuration = 3;
  host.autoResumeDuration = 3;
  host.autoResumeNumberOfSegments = 1;
  host.segmentRequestRetryLimit = 10;

  host.onError = onHostError;
  host.onAutoPause = onHostAutoPause;

  return host;
}

function onHostError(errorCode, request) {
  unloadPlayer();

  var errorString = 'Fatal Error: ' + errorCode +
                    ' (' + request.errorCode + ', ' + request.status + ')';
  console.error(errorString);
  notice.text(errorString);
  notice.show();
}

function onHostAutoPause(isPaused) {
  if (isPaused) {
    notice.text("Auto paused");
    notice.show();
  }
  else {
    notice.hide();
  }
}

function unloadPlayer() {
  if (player !== null) {
    player.unload();
    player = null;
  }
}
