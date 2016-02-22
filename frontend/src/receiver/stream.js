var streamInfoContainer = $('#stream-info');

var REFRESH_INFO_INTERVAL = 30; // seconds
var refreshId = setInterval(refreshStreamInfo, REFRESH_INFO_INTERVAL * 1000);

function toggleStreamInfo(visible) {
  streamInfoContainer.toggle(visible);
}

function refreshStreamInfo() {
  $.ajax({
    url: 'https://api.twitch.tv/kraken/streams/' + currentChannel,
    success: updateInfo
  })
}

function updateInfo(data) {
  streamInfoContainer.html('📺 ' + data.stream.viewers.toLocaleString() + '   /   ' +
                           '👀 ' + data.stream.channel.views.toLocaleString() + '   /   ' +
                           '❤ ' + data.stream.channel.followers.toLocaleString());
}
