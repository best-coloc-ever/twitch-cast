<chat-line>

  <li>
    <span class="badge-wrapper" if={ badges.length > 0 }>
      <div>
        <img each={ badge in badges } src={ store.badges[badge] }>
      </div>
    </span>
    <span class="sender" style="color: { color }">{ sender }</span>
    :
    <span class="message">
      <virtual each={ part in content }>
        <virtual if={ part.type == 'word' }>{ part.word }</virtual>
        <img if={ part.type == 'img' } src={ part.src }>
      </virtual>
    </span>
  </li>

  <style scoped>
    li {
      font-family: Helvetica Neue,Helvetica,sans-serif;
      font-size: 12px;
      line-height: 20px;
      color: #8c8c9c;
      list-style-type: none;
    }

    .sender {
      font-weight: bold;
    }

    .badge-wrapper {
      float: left;
      min-width: 20px;
      vertical-align: middle;
    }

    img {
      vertical-align: middle;
    }

    li.show {
      opacity: 1;
    }
  </style>

  <script>
    var DEFAULT_COLORS = [
      '#FF0000', '#0000FF', '#00FF00', '#B22222', '#FF7F50',
      '#9ACD32', '#FF4500', '#2E8B57', '#DAA520', '#D2691E',
      '#5F9EA0', '#1E90FF', '#FF69B4', '#8A2BE2', '#00FF7F'
    ];
    var BADGES_TYPES = ['mod', 'subscriber', 'turbo'];

    var self = this;

    this.store = opts.store;
    this.sender = opts.message.sender;
    this.tags = (opts.message.tags || {});
    this.color = (this.tags.color || defaultColor(this.sender));
    this.content = opts.message.content.split(' ').map(messagePart)
    this.badges = $.unique(BADGES_TYPES.filter(function(type) {
      return (self.tags[type] == '1');
    }));

    // https://discuss.dev.twitch.tv/t/default-user-color-in-chat/385
    function defaultColor(name) {
      var n = name.charCodeAt(0) + name.charCodeAt(name.length - 1);

      return DEFAULT_COLORS[n % DEFAULT_COLORS.length];
    }

    function messagePart(word) {
      var url = self.store.emotes[word];

      if (url)
        return { type: 'img', src: url };

      return { type: 'word', word: word };
    }

  </script>

</chat-line>
