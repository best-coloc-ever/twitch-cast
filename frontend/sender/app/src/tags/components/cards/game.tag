<game-card>

  <!-- layout -->
  <a href={ '#' + gameLink }>
    <div class="preview">
      <img src={ data.box.large }>
      <div>
        <div class="game-name">{ data.name }</div>
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
  </style>


  <!-- logic -->
  <script>
    import { routeLinks } from 'routing/routes.js'

    this.data = opts.data
    this.gameLink = routeLinks.game(this.data.name)
  </script>

</game-card>
