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

  self.badges['alliance'] = 'https://static-cdn.jtvnw.net/badges/v1/c4816339-bad4-4645-ae69-d1ab2076a6b0/1';
  self.badges['horde'] = 'https://static-cdn.jtvnw.net/badges/v1/de8b26b6-fd28-4e6c-bc89-3d597343800d/1';

  self.badges['bits1'] = 'https://static-cdn.jtvnw.net/badges/v1/73b5c3fb-24f9-4a82-a852-2f475b59411c/1'
  self.badges['bits100'] = 'https://static-cdn.jtvnw.net/badges/v1/09d93036-e7ce-431c-9a9e-7044297133f2/1'
  self.badges['bits1000'] = 'https://static-cdn.jtvnw.net/badges/v1/0d85a29e-79ad-4c63-a285-3acd2c66f2ba/1'
  self.badges['bits5000'] = 'https://static-cdn.jtvnw.net/badges/v1/57cd97fc-3e9e-4c6d-9d41-60147137234e/1'
  self.badges['bits10000'] = 'https://static-cdn.jtvnw.net/badges/v1/68af213b-a771-4124-b6e3-9bb6d98aa732/1'
  self.badges['bits100000'] = 'https://static-cdn.jtvnw.net/badges/v1/96f0540f-aa63-49e1-a8b3-259ece3bd098/1'

  var CHEER_COLORS_BY_MILESTONE = [
    [100000, 'gold'],
    [10000,  'red'],
    [5000,   'blue'],
    [1000,   'green'],
    [100,    'purple'],
    [1,      'gray']
  ];
  this.cheerColor = function(cheerCount) {
    for (var [milestone, color] of CHEER_COLORS_BY_MILESTONE)
      if (cheerCount >= milestone)
        return color;
  }

  var CHEER_THEME = 'dark';
  var CHEER_TYPE = 'animated';
  var CHEER_SIZE = '1';
  this.cheerEmote = function(cheerCount) {
    return 'https://static-cdn.jtvnw.net/bits/' +
            CHEER_THEME + '/' + CHEER_TYPE + '/' +
            self.cheerColor(cheerCount) + '/' + CHEER_SIZE;
  }

  // Global emotes
  $.ajax({
    url: 'https://twitchemotes.com/api_cache/v2/global.json',
    cache: false,
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
    cache: false,
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
