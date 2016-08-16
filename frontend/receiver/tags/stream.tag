<stream>

  <!-- Layout -->
  <notice></notice>
  <pause-indicator></pause-indicator>

  <div class={ center: videojsPlayer }>
    <video autoplay></video>
  </div>

  <clock if={ showStreamInfos }></clock>
  <stream-info if={ showStreamInfos }></stream-info>

  <!-- Style -->
  <style scoped>
    notice {
      position: absolute;
      text-align: center;
      color: white;
      width: 100%;
    }

    pause-indicator {
      position: absolute;
      top: 0;
      bottom: 0;
      left: 0;
      right: 0;
    }

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
      var video = this.mediaElement();

      var playerOptions = {
        nativeControlsForTouch: true,
        preload: true
      };
      this.videojsPlayer = videojs(video, playerOptions, function() {
        this.play();
      });

      this.videojsPlayer.src({
        src: url,
        type: 'application/vnd.apple.mpegurl'
      });

      video.setAttribute("controls","controls");

      this.update();
    }

    notice(e) {
      if (e.hide)
        this.tags.notice.hide();
      else
        this.tags.notice.show(e.text);

      this.tags['pause-indicator'].setVisible((e.text == 'Auto paused'));
    }

  </script>

</stream>
