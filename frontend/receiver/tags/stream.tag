<stream>

  <!-- Layout -->
  <div class={ center: videojsPlayer }>
    <video autoplay></video>
  </div>

  <clock if={ showStreamInfos }></clock>
  <stream-info if={ showStreamInfos }></stream-info>

  <!-- Style -->
  <style scoped>
    video {
      width: 100%;
      height: 100%;
      margin: auto auto;
      overflow: hidden !important;
    }

    .center {
      position: relative;
      top: 50%;
      transform: translateY(-50%);
    }

    .vjs_video_3-dimensions {
      width: 100% !important;
      height: 100% !important;
    }

    stream-info {
      position: absolute;
      bottom: 2%;
      right: 2%;
      text-align: right;
      color: white;
    }

    clock {
      position: absolute;
      bottom: 2%;
      left: 2%;
      text-align: left;
      color: white;
    }
  </style>

  <!-- Logic -->
  <script>
    this.showStreamInfos = true;
    this.videojsPlayer = null;

    mediaElement() {
      return $(this.root).find('video')[0];
    }

    fullScreen(on) {
      this.showStreamInfos = !on;
    }

    setChannel(channel) {
      this.tags['stream-info'].setChannel(channel);
    }

    setDesktopSource(url) {
      var source = document.createElement('source');
      source.src = url;
      source.type = 'application/vnd.apple.mpegurl';

      var video = this.mediaElement();
      video.appendChild(source);

      var playerOptions = {
        nativeControlsForTouch: true,
      };
      this.videojsPlayer = videojs(video, playerOptions, function() {
        this.play();
      });

      this.update();
    }
  </script>

</stream>
