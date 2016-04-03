<chat-line>

  <li>
    <span class="badge-wrapper" if={ badges.length > 0 }>
      <div each={ badge in badges }>
        <img src={ store.badges[badge] }>
      </div>
    </span>
    <span class="sender" style="color: { color }">{ sender }</span>
    :
    <span class="message">
      <!-- How do I even generate the html I need here ? -->
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
  </style>

  <script>
    var DEFAULT_COLORS = [
      '#FF0000', '#0000FF', '#00FF00', '#B22222', '#FF7F50',
      '#9ACD32', '#FF4500', '#2E8B57', '#DAA520', '#D2691E',
      '#5F9EA0', '#1E90FF', '#FF69B4', '#8A2BE2', '#00FF7F'
    ];
    var BADGES_TYPES = ['mod', 'subscriber', 'turbo'];

    var self = this;

    this.sender = opts.message.sender;
    this.tags = (opts.message.tags || {});
    this.assetStore = this.parent.assetStore;
    this.badges = $.unique(BADGES_TYPES.filter(function(type) {
      return (self.tags[type] == '1');
    }));
    this.color = (this.tags.color || defaultColor(this.sender));

    // :'(
    var content = opts.message.content.split(' ').map(function(word) {
      if (word in self.assetStore.emotes)
        return '<img src="' + self.assetStore.emotes[word] + '">'
      return word;
    }).join(' ');

    this.on('mount', function() {
      $(this.root).find('.message').html(content);
    })
    //

    // https://discuss.dev.twitch.tv/t/default-user-color-in-chat/385
    function defaultColor(name) {
      var n = name.charCodeAt(0) + name.charCodeAt(name.length - 1);

      return DEFAULT_COLORS[n % DEFAULT_COLORS.length];
    }
  </script>

</chat-line>
