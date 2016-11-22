<search-view>

  <card-list-view config={ config }>
  </card-list-view>

  <script>
    import TwitchAPI from 'api/twitch.js'

    let fetchLogic = params => {
      let newParams = Object.assign(
        params,
        { q: opts.routeOpts[0] }
      )

      return TwitchAPI.Search.streams(newParams)
    }

    this.config = {
      fetchLogic: fetchLogic,
      dataFilter: data => data.streams,
      cardTag: 'stream-card',
      tagOpts: { sender: opts.sender },
      rowSizes: {
        desktop: 4,
        tablet:  2,
        phone:   1,
      },
    }
  </script>

</search-view>
