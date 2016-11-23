<channels-view>

  <card-list-view config={ config }>
  </card-list-view>

  <script>
    import TwitchAPI from 'api/twitch.js'

    let [game] = opts.path

    let fetchLogic = params => {
      if (game)
        params.game = game

      return TwitchAPI.streams(params)
    }

    this.config = {
      fetchLogic: fetchLogic,
      dataFilter: data => data.streams,
      cardTag: 'stream-card',
      rowSizes: {
        desktop: 4,
        tablet:  2,
        phone:   1,
      },
    }
  </script>

</channels-view>
