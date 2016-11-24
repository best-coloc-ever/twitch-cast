<following-view>

  <h5 align="center" if={ !authToken }>You are not logged in to Twitch</h5>

  <card-list-view config={ config } if={ authToken }>
  </card-list-view>

  <!-- logic -->
  <script>
    import TwitchAPI from 'api/twitch.js'

    this.authToken = TwitchAPI.getToken()

    this.config = {
      fetchLogic: params => TwitchAPI.followed(this.authToken, params),
      dataFilter: data => data.streams,
      cardTag: 'stream-card',
      rowSizes: {
        desktop: 4,
        tablet:  2,
        phone:   1,
      },
    }
  </script>

</following-view>
