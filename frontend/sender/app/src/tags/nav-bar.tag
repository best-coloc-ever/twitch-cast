<nav-bar>

  <!-- layout -->
  <span class="mdl-layout-title mdl-color-text--primary-contrast">
    Browse
  </span>

  <nav class="mdl-navigation">

    <a class="mdl-navigation__link mdl-color-text--primary-contrast test"
       href="#channels">
      <i class="material-icons">videocam</i><span>Channels</span>
    </a>
  </nav>

  <!-- style -->
  <style scoped>
    .material-icons {
      padding-right: 10px;
    }

    .mdl-navigation__link:hover {
      background-color: #673ab7 !important;
    }
  </style>

  <!-- logic -->
  <script>
    this.on('mount', () => {

    })
  </script>

</nav-bar>
