<pause-indicator>

  <img class="rotate" src="/twitch-cast/img/kappa.png" if={ visible }>

  <style scoped>
    img {
      position: absolute;
      margin: auto;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      z-index: 42;
    }

    @-webkit-keyframes rotate {
      from {
        -webkit-transform: rotate(0deg);
      }
      to {
        -webkit-transform: rotate(360deg);
      }
    }

    .rotate {
      -webkit-animation: rotate 1s linear infinite;
    }
  </style>

  <script>
    this.visible = false

    this.setVisible = (visible) => {
      this.visible = visible
      this.update()
    }
  </script>

</pause-indicator>
