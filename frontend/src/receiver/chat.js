
var chatContainer = $('#chat');
var chatLines = $('#chat-lines');

var chatAssetStore = new ChatAssetStore();

var BadgeStore = function(channel) {
  var self = this;

  $.ajax({
    url: 'https://api.twitch.tv/kraken/chat/lirik/badges',
    success: function(data) {
      self.globalMod = data.global_mod.image
      self.admin = data.admin.image
      self.broadcaster = data.broadcaster.image
      self.mod = data.mod.image
      self.staff = data.staff.image
      self.turbo = data.turbo.image
      self.subscriber = data.subscriber.image
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

function toggleChat(visible) {
  chatContainer.toggle(visible);
}

function connectToChat(channel) {

}
