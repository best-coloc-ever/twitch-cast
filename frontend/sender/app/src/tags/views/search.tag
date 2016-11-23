<search-view>

  <div class="mdl-tabs mdl-js-tabs mdl-js-ripple-effect">

    <div class="mdl-tabs__tab-bar">
      <a href="#streams-panel" class="mdl-tabs__tab is-active">Streams</a>
      <a href="#games-panel" class="mdl-tabs__tab">Games</a>
      <a href="#channels-panel" class="mdl-tabs__tab">Channels</a>
    </div>

    <card-list-view
      class="mdl-tabs__panel is-active" id="streams-panel"
      config={ streamsConfig }>
    </card-list-view>

    <card-list-view
      class="mdl-tabs__panel" id="games-panel"
      config={ gamesConfig }>
    </card-list-view>

    <card-list-view
      class="mdl-tabs__panel" id="channels-panel"
      config={ channelsConfig }>
    </card-list-view>

  </div>

  <script>
    import TwitchAPI from 'api/twitch.js'

    let [query] = opts.path

    const streamsRowSizes =  {desktop: 4, tablet: 2, phone: 1 }
    const gamesRowSizes =    {desktop: 6, tablet: 4, phone: 2 }
    const channelsRowSizes = streamsRowSizes

    let pagedQueryFetchLogic = call => params =>
      call(Object.assign(params, { q: query }))

    let gamesQueryFetchLogic = call => _ =>
      call({ q: query, type: 'suggest' })

    this.streamsConfig = {
      fetchLogic: pagedQueryFetchLogic(TwitchAPI.Search.streams),
      dataFilter: data => data.streams,
      cardTag: 'stream-card',
      rowSizes: streamsRowSizes,
    }

    this.gamesConfig = {
      fetchLogic: gamesQueryFetchLogic(TwitchAPI.Search.games),
      dataFilter: data => data.games,
      cardTag: 'game-card',
      rowSizes: gamesRowSizes,
    }

    this.channelsConfig = {
      fetchLogic: pagedQueryFetchLogic(TwitchAPI.Search.channels),
      dataFilter: data => data.channels,
      cardTag: 'channel-card',
      rowSizes: channelsRowSizes,
    }

    this.on('mount', () => {
      componentHandler.upgradeElements(this.root)
    })
  </script>

</search-view>
