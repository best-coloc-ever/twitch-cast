var DEFAULT_COLORS = [
  '#FF0000', '#0000FF', '#00FF00', '#B22222', '#FF7F50',
  '#9ACD32', '#FF4500', '#2E8B57', '#DAA520', '#D2691E',
  '#5F9EA0', '#1E90FF', '#FF69B4', '#8A2BE2', '#00FF7F'
];

var BADGE_TYPES = [
  'global_mod', 'admin', 'broadcaster',
  'mod', 'staff', 'turbo', 'subscriber'
]

// https://discuss.dev.twitch.tv/t/default-user-color-in-chat/385
function defaultColor(name) {
  var n = name.charCodeAt(0) + name.charCodeAt(name.length - 1);

  return DEFAULT_COLORS[n % DEFAULT_COLORS.length];
}

function getBadges(message) {
  var mTags = (message.tags || {});
  var badgeMap = {};
  var badges = [];

  if (mTags.badges)
    mTags.badges.split(',').forEach(function(badge) {
      var splitted = badge.split('/');
      badgeMap[splitted[0]] = splitted[1];
    });

  BADGE_TYPES.forEach(function(type) {
    if (badgeMap[type] == '1')
      badges.push(type);
  });
  if (badgeMap.moderator) // I noticed this inconsistency...
    badges.push('mod');
  if (badgeMap.warcraft)
    badges.push(badgeMap.warcraft);
  if (badgeMap.bits)
    badges.push('bits' + badgeMap.bits)
  return badges;
}

function buildChatLine(message, store) {
  var li = $('<li>')
    .addClass('chat-line');

  var badges = getBadges(message);
  if (badges.length) {
    var badgeSpan = $('<span>')
      .addClass('badge-wrapper');

    var div = $('<div>')
      .addClass('badge-container');
    for (var badge of badges) {
      var img = $('<img>')
        .attr('src', store.badges[badge])
        .addClass('badge');

      div.append(img);
    }
    badgeSpan.append(div);

    li.append(badgeSpan);
  }

  var mTags = (message.tags || {});
  var color = (mTags.color || defaultColor(message.sender));

  var nameSpan = $('<span>')
    .addClass('chat-line-sender')
    .text(message.sender)
    .css('color', color);

  var htmlParts = message.content.split(' ').map(function(word) {
    if (word in store.emotes)
      return '<img src="' + store.emotes[word] + '">';

    return word;
  });

  var contentSpan = $('<span>').html(': ' + htmlParts.join(' '));

  li.append(nameSpan);
  li.append(contentSpan);

  return li
}
