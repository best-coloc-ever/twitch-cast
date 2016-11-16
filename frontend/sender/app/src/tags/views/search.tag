<search-view>

  <!-- layout -->
  <div class="mdl-grid stream-grid" each={ streams in streamsChunks }>
    <div class="mdl-cell mdl-cell--3-col mdl-cell--4-col-tablet mdl-cell--12-col-phone"
         each={ stream in streams }>
      <stream-card data={ stream } sender={ sender }></stream-card>
    </div>
  </div>


  <div class="bottom-wrapper">
    <div
      class="mdl-progress mdl-js-progress mdl-progress__indeterminate progress-bar"
      show={ loading }>
    </div>
    <button
      class="mdl-button mdl-js-button mdl-button--raised mdl-button--colored"
      onclick={ fetchNextStreams }
      show={ !loading }>
      Load more
    </button>
  </div>


  <!-- style -->
  <style scoped>
    .bottom-wrapper {
      text-align: center;
      padding-top: 15px;
      padding-bottom: 15px;
    }

    .progress-bar {
      margin: 0 auto;
    }

    .stream-grid {
      padding: 0;
    }
  </style>


  <!-- logic -->
  <script>
    import TwitchAPI from 'api/twitch.js'

    const streamChunkSize = 4
    const streamFetchCount = 40

    this.streamsChunks = []
    this.offset = 0
    this.loading = false
    this.sender = opts.sender
    this.query = opts.routeOpts ? opts.routeOpts[0] : null

    this.addStreams = data => {
      let streams = data.streams
      let i = 0
      // Make sure we fill out the last chunk if any
      if (this.streamsChunks.length) {
        let lastChunkIdx = this.streamsChunks.length - 1
        let lastChunk = this.streamsChunks[lastChunkIdx]

        while (lastChunk.length < streamChunkSize && i < streams.length)
          lastChunk.push(streams[i++])
      }
      // Split the remaining streams
      while (i < streams.length) {
        this.streamsChunks.push(streams.slice(i, i + streamChunkSize))
        i += streamChunkSize
      }
    }

    this.fetchNextStreams = () => {
      this.loading = true
      this.update()

      let params = {
        q: this.query,
        offset: this.offset,
        limit: streamFetchCount,
      }

      TwitchAPI.Search.streams(params)
        .then(data => {
          this.offset += data.streams.length
          this.addStreams(data)
          this.loading = false
          this.update()
        })
    }

    this.on('mount', () => {
      componentHandler.upgradeElements(this.root)

      this.fetchNextStreams()
    })
  </script>

</search-view>
