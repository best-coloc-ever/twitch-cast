var BadgeStore = function(channel) {
  var self = this;

  $.ajax({
    url: 'https://api.twitch.tv/kraken/chat/' + channel + '/badges',
    success: function(data) {
      for (var key in data)
        self[key] = data[key].image;
    }
  })
}

var ChatAssetStore = function() {
  var self = this;

  this.emotes = {};
  this.badges = {};

  this.fetchBadges = function(channel) {
    this.badges[channel] = new BadgeStore(channel);
  }

  // Global emotes
  $.ajax({
    url: 'http://twitchemotes.com/api_cache/v2/global.json',
    success: function(data) {
      var template = data.template.small;

      for (var key in data.emotes) {
        var imageId = data.emotes[key].image_id.toString();
        self.emotes[key] = template.replace('{image_id}', imageId);
      }
    }
  });
  // Subscriber emotes
  $.ajax({
    url: 'http://twitchemotes.com/api_cache/v2/subscriber.json',
    success: function(data) {
      var template = data.template.small;

      for (var channel in data.channels) {
        var emotes = data.channels[channel].emotes;

        for (var i = 0; i < emotes.length; ++i) {
          var code = emotes[i].code;
          var imageId = emotes[i].image_id.toString();
          self.emotes[code] = template.replace('{image_id}', imageId);
        }
      }
    }
  });
  // BTTV emotes
  $.ajax({
    url: 'https://api.betterttv.net/2/emotes',
    success: function(data) {
      var template = data.urlTemplate;

      for (var i = 0; i < data.emotes.length; i++) {
        var emote = data.emotes[i];
        var code = emote.code;
        self.emotes[code] = template.replace('{{id}}', emote.id)
                                    .replace('{{image}}', '1x');
      }
    }
  });
}

var CHAT_DELAY = 0; // seconds

var currentChannel = null;
var chatSocket = null;

var chatContainer = $('#chat');
var chatLines = $('#chat-lines');

var chatAssetStore = new ChatAssetStore();

function toggleChat(visible) {
  chatContainer.toggle(visible);
}

function displayChatRaw(text) {
  var line = $('<li>').text(text);
  chatLines.append(line);
}

function connectToChat(channel) {
  chatAssetStore.fetchBadges(channel);

  // Close any previous connection
  if (chatSocket)
    chatSocket.close();

  // Clear chat's content
  chatLines.empty();
  // Attempt to connect to the new channel
  displayChatRaw('Joining chat: ' + channel + ' ...');

  chatSocket = new WebSocket('ws://' + window.location.host + '/chat/' + channel);

  chatSocket.onopen = function(e) {
    currentChannel = channel;
    displayChatRaw('Successfully joined the channel!');
    displayChatRaw('Delaying chat for: ' + CHAT_DELAY + ' seconds')
  };

  chatSocket.onerror = function(e) {
    displayChatRaw('Error: ' + e.reason);
  };

  chatSocket.onmessage = function(e) {
    var message = JSON.parse(e.data);
    setTimeout(displayPrivateMessage.bind(null, message), CHAT_DELAY * 1000);
  };
}

function displayPrivateMessage(message) {
  var color = message.tags ? message.tags.color : 'blue';

  var line = $('<li>')
    .addClass('chat-line');
  var nameSpan = $('<span>')
    .addClass('chat-line-sender')
    .text(message.sender)
    .css('color', color);
  var contentSpan = makeContentSpan(message.content);

  prependBadges(message.tags, line);
  line.append(nameSpan);
  line.append(contentSpan);
  chatLines.append(line);

  chatContainer.scrollTop(chatContainer[0].scrollHeight);
}

function makeContentSpan(content) {
  var htmlParts = content.split(' ').map(function(word) {
    if (word in chatAssetStore.emotes)
      return '<img src="' + chatAssetStore.emotes[word] + '">';

    return word;
  });
  return $('<span>').html(': ' + htmlParts.join(' '));
}

function prependBadges(tags, line) {
  if (tags) {
    var specialAttributes = ['mod', 'subscriber', 'turbo']
      .filter(function(attribute) {
        return (tags[attribute] == '1');
      });

    if (tags['user-type'])
      specialAttributes.push(tags['user-type']);

    if (specialAttributes.length > 0) {
      var badgeWrapper = $('<span>')
        .addClass('badge-wrapper');
      var badgeContainer = $('<div>');

      $.unique(specialAttributes).forEach(function(attribute) {
        var badge = $('<img>')
          .attr('src', chatAssetStore.badges[currentChannel][attribute]);
        badgeContainer.append(badge);
      })

      badgeWrapper.wrapInner(badgeContainer);
      line.append(badgeWrapper);
    }

  }
}
