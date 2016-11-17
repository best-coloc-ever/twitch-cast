<loading-view>

   <!-- layout -->
  <div id="main">

    <div if={ loading }>
      <div class="mdl-grid">
        <div class="mdl-layout-spacer"></div>
        <div>
          <span class="mdl-color-text--primary" id="loading">Loading </span>
          <span class="mdl-color-text--primary" id="channel">{ channel }</span>
        </div>
        <div class="mdl-layout-spacer"></div>
      </div>

      <div class="mdl-progress mdl-js-progress mdl-progress__indeterminate"></div>
    </div>

    <div if={ !loading }>
      <div class="mdl-grid">
        <div class="mdl-layout-spacer"></div>
        <div>
          <span class="mdl-color-text--primary" id="loading">Encountered an error while loading </span>
          <span class="mdl-color-text--primary" id="channel">{ channel }</span>
          <br>
          <span class="mdl-color-text--primary">The stream is probably offline</span>
          <br>
          <span class="mdl-color-text--primary">Trying again in { secondsBeforeRetry } seconds</span>
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

    #loading {
      font-size: 30px;
      font-weight: 700;
      /*text-transform: uppercase;*/
      white-space: nowrap;
    }

    #channel {
      font-size: 30px;
      font-weight: 700;
      text-transform: uppercase;
    }
  </style>


  <!-- logic -->
  <script>
    import StreamerAPI from 'api/streamer.js'

    let retryInSeconds = 5
    let secondsRefreshTimer = null
    let [channel] = opts.routeOpts

    this.channel = channel
    this.loading = true
    this.secondsBeforeRetry = retryInSeconds

    this.onStreamFetched = data => {
      let playlists = data.playlists
      // Sort by bandwidth descending
      playlists.sort((a, b) => b.bandwidth - a.bandwidth)
      // Choosing the best quality one
      let quality = data.playlists[0].name

      fetch(StreamerAPI.playlistUrl(channel, quality))
        .then(
          response => {
            if (response.ok)
              this.onPlaylistFetched(quality)
            else
              this.onError()
          },
          this.onError
        )
    }

    this.onPlaylistFetched = quality => {
      riot.route(`/${channel}/${quality}`, `${channel} (${quality})`, true)
    }

    this.onError = () => {
      this.loading = false

      clearInterval(secondsRefreshTimer)
      this.secondsBeforeRetry = retryInSeconds

      setTimeout(this.loadStream, retryInSeconds * 1000)
      retryInSeconds *= 2

      secondsRefreshTimer = setInterval(() => {
        this.secondsBeforeRetry -= 1
        this.update()
      }, 1000)

      this.update()
    }

    this.loadStream = () => {
      this.loading = true

      StreamerAPI.stream(channel)
        .then(this.onStreamFetched, this.onError)

      this.update()
    }

    this.on('mount', () => {
      componentHandler.upgradeElements(this.root)

      this.loadStream()
    })
  </script>

</loading-view>
