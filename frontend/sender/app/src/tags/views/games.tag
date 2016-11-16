<games-view>

  <!-- layout -->
  <div class="mdl-grid games-grid" each={ games in gamesChunks }>
    <div class="mdl-cell mdl-cell--2-col mdl-cell--2-col-tablet mdl-cell--2-col-phone"
         each={ game in games }>
      <game-card data={ game }></game-card>
    </div>
  </div>

  <div class="bottom-wrapper">
    <div
      class="mdl-progress mdl-js-progress mdl-progress__indeterminate progress-bar"
      show={ loading }>
    </div>
    <button
      class="mdl-button mdl-js-button mdl-button--raised mdl-button--colored"
      onclick={ fetchNextGames }
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

    .games-grid {
      padding: 0;
    }
  </style>


  <!-- logic -->
  <script>
    import TwitchAPI from 'api/twitch.js'

    const gamesChunkSize = 12
    const gamesFetchCount = 48

    this.gamesChunks = []
    this.offset = 0
    this.loading = false

    this.addGames = data => {
      let games = data.top
      let i = 0
      // Make sure we fill out the last chunk if any
      if (this.gamesChunks.length) {
        let lastChunkIdx = this.gamesChunks.length - 1
        let lastChunk = this.gamesChunks[lastChunkIdx]

        while (lastChunk.length < gamesChunkSize && i < games.length)
          lastChunk.push(games[i++])
      }
      // Split the remaining games
      while (i < games.length) {
        this.gamesChunks.push(games.slice(i, i + gamesChunkSize))
        i += gamesChunkSize
      }
    }

    this.fetchNextGames = () => {
      this.loading = true
      this.update()

      TwitchAPI.games({ offset: this.offset, limit: gamesFetchCount })
        .then(data => {
          this.offset += data.top.length
          this.addGames(data)
          this.loading = false
          this.update()
        })
    }

    this.on('mount', () => {
      componentHandler.upgradeElements(this.root)

      this.fetchNextGames()
    })
  </script>

</games-view>
