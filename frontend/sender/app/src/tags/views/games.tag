<games-view>

  <card-list-view config={ config }>
  </card-list-view>

  <script>
    import TwitchAPI from 'api/twitch.js'

    this.config = {
      fetchLogic: TwitchAPI.games,
      dataFilter: data => data.top,
      cardTag: 'top-game-card',
      tagOpts: { },
      rowSizes: {
        desktop: 6,
        tablet:  4,
        phone:   2,
      },
    }
  </script>

</games-view>
