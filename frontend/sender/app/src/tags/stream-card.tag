<stream-card>

  <!-- layout -->
  <div class="preview">
    <img src={ data.preview.medium }>
    <div class="embed">
      <div class="streamer-name">{ data.channel.display_name }</div>
      <div class="stream-game">
        Playing
        <span class="game-name">{ data.game }</span>
      </div>
      <div class="viewers">
        <span class="viewer-count">{ data.viewers }</span>
        viewers
      </div>
    </div>
  </div>


  <!-- style -->
  <style scoped>
    .preview {
      position: relative;
      background: linear-gradient(rgba(255, 0, 0, 0), rgba(0, 0,0 , 0.8))
    }

    img {
      width: 100%;
      position: relative;
      z-index: -1;
    }

    .embed {
      position: absolute;
      bottom: 2%;
      left: 2%;
      width: 90%;
      overflow: hidden;
    }

    .streamer-name {
      font-weight: 700;
      font-size: large;
      color: white;
    }

    .stream-game {
      color: #b186ff;
      white-space: nowrap;
    }

    .game-name {
      font-weight: bold;
    }

    .viewers {
      color: #b186ff;
      white-space: nowrap;
    }

    .viewer-count {
      font-weight: bold;
    }
  </style>


  <!-- logic -->
  <script>
    this.data = opts.data

    this.on('mount', () => {

    })
  </script>

</stream-card>
