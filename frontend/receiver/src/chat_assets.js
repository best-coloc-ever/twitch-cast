var ChatAssetStore = function(channel) {
  var self = this;

  this.emotes = {};
  this.badges = {};

  // Badges
  $.ajax({
    url: 'https://api.twitch.tv/kraken/chat/' + channel + '/badges',
    success: function(data) {
      for (var key in data)
        if (data[key])
          self.badges[key] = data[key].image;
    }
  })

  // Global emotes
  $.ajax({
    url: 'https://twitchemotes.com/api_cache/v2/global.json',
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
    url: 'https://twitchemotes.com/api_cache/v2/subscriber.json',
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

      // Some more
      $.ajax({
        url: 'https://raw.githubusercontent.com/Jiiks/BetterDiscordApp/master/data/emotedata_bttv.json',
        success: function(data) {
          data = JSON.parse(data); // Not parsed for some reason. I guess headers

          for (var code in data) {
            self.emotes[code] = template.replace('{{id}}', data[code])
                                        .replace('{{image}}', '1x');
          }
        }
      })
    }
  });
}
