<channel-card>

  <!-- layout -->
  <div class="preview" onclick={ onClick }>
    <img src={ logo }>
    <div class="embed">
      <div class="channel-name">{ data.display_name }</div>
    </div>
  </div>


  <!-- style -->
  <style scoped>
    .preview {
      cursor: pointer;
      position: relative;
      background: linear-gradient(rgba(255, 0, 0, 0) 50%, rgba(0, 0, 0, 0.8))
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

    .channel-name {
      font-weight: 700;
      font-size: large;
      color: white;
    }
  </style>


  <!-- logic -->
  <script>
    this.data = opts.data

    this.logo = this.data.logo || '//fakeimg.pl/300x300/?text=?'

    this.onClick = () => {
      opts.sender.play(this.data.name)
    }

    this.on('mount', () => {

    })
  </script>

</channel-card>
