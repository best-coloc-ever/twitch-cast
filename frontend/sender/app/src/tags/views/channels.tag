<channels-view>

  <card-list-view config={ config }>
  </card-list-view>

  <script>
    import TwitchAPI from 'api/twitch.js'

    this.config = {
      fetchLogic: TwitchAPI.streams,
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

</channels-view>
