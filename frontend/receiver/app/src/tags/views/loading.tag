<loading-view>

   <!-- layout -->
  <div id="main">

    <div show={ loading }>
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

    <div show={ !loading }>
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

    import { routeLinks } from 'routing/routes.js'

    let retryInSeconds = 5
    let secondsRefreshTimer = null
    let [channel, quality] = opts.path

    this.channel = channel
    this.loading = true
    this.secondsBeforeRetry = retryInSeconds

    this.onStreamFetched = data => {
      let playlists = data.playlists
      // Sort by bandwidth descending
      playlists.sort((a, b) => b.bandwidth - a.bandwidth)
      // Choosing the best quality one
      let quality = data.playlists[0].name

      this.fetchPlaylist(quality)
    }

    this.onPlaylistFetched = quality => {
      riot.route(
        routeLinks.watch(channel, quality),
        `${channel} (${quality})`,
        true
      )
    }

    this.onError = () => {
      clearInterval(secondsRefreshTimer)

      setTimeout(this.loadStream, retryInSeconds * 1000)
      retryInSeconds *= 2

      secondsRefreshTimer = setInterval(
        () => this.update({ secondsBeforeRetry: this.secondsBeforeRetry - 1 }),
        1000
      )

      this.update({ secondsBeforeRetry: retryInSeconds, loading: false })
    }

    this.fetchPlaylist = quality => {
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

    this.loadStream = () => {
      StreamerAPI.stream(channel)
        .then(this.onStreamFetched, this.onError)

      this.update({ loading: true })
    }

    this.on('mount', () => {
      componentHandler.upgradeElements(this.root)

      if (quality)
        this.fetchPlaylist(quality)
      else
        this.loadStream()
    })

    this.on('unmount', () => {
      clearInterval(secondsRefreshTimer)
    })
  </script>

</loading-view>
