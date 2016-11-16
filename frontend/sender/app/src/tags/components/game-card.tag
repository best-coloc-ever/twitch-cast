<game-card>

  <!-- layout -->
  <a href={ gameLink }>
    <div class="preview">
      <img src={ data.game.box.large }>
      <div>
        <div class="game-name">{ data.game.name }</div>
        <div class="viewers">{ data.viewers } viewers / { data.channels } channels</div>
        <div></div>
      </div>
    </div>
  </a>


  <!-- style -->
  <style scoped>
    a {
      text-decoration: none;
      color: unset;
    }

    .preview {
      position: relative;
    }

    img {
      width: 100%;
      position: relative;
      z-index: -1;
    }

    .game-name {
      white-space: nowrap;
      overflow-x: hidden;
    }

    .viewers {
      font-size: small;
      color: gray;
      white-space: nowrap;
      overflow-x: hidden;
    }
  </style>


  <!-- logic -->
  <script>
    this.data = opts.data
    this.gameLink = `#games/${this.data.game.name}`

    this.on('mount', () => {

    })
  </script>

</game-card>
