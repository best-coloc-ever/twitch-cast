<home-view>

  <!-- layout -->
  <div id="main">
    <div>

      <div class="mdl-grid">
        <div class="mdl-layout-spacer"></div>
        <div>
          <span class="mdl-color-text--primary" id="title">Twitch Caster</span>
        </div>
        <div class="mdl-layout-spacer"></div>
      </div>

      <div class="mdl-grid">
        <div class="mdl-layout-spacer"></div>
        <div>
          <span class="mdl-color-text--primary">Ready to cast</span>
        </div>
        <div class="mdl-layout-spacer"></div>
      </div>

    </div>
  </div>


  <!-- style -->
  <style scoped>
    #main {
      height: 100%;
      display: flex;
      align-items: center;
      justify-content: center;
    }

    #title {
      font-size: 40px;
      font-weight: 900;
      text-transform: uppercase;
      white-space: nowrap;
    }
  </style>


  <!-- logic -->
  <script>
    this.on('mount', () => {

    })
  </script>

</home-view>
