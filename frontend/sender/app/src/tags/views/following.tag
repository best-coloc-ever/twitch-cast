<following-view>

  <card-list-view config={ config }>
  </card-list-view>

  <!-- logic -->
  <script>
    import TwitchAPI from 'api/twitch.js'
    import Cookies from 'js-cookie'

    let authToken = Cookies.get('twitch-oauth-token')

    this.config = {
      fetchLogic: TwitchAPI.followed.bind(undefined, authToken),
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
