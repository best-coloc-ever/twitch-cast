<stream>

  <!-- Layout -->
  <video autoplay></video>

  <stream-info if={ showStreamInfos }></stream-info>

  <!-- Style -->
  <style scoped>
    video {
      width: 100%;
      height: 100%;
      margin: auto auto;
      overflow: hidden !important;
    }

    stream-info {
      position: absolute;
      bottom: 2%;
      right: 2%;
      text-align: right;
      color: white;
    }
  </style>

  <!-- Logic -->
  <script>
    this.showStreamInfos = true;

    mediaElement() {
      return $(this.root).find('video')[0];
    }

    fullScreen(on) {
      this.showStreamInfos = !on;
    }

    setChannel(channel) {
      this.tags['stream-info'].setChannel(channel);
    }
  </script>

</stream>
