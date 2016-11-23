<twitch-login-button>

  <!-- layout -->
  <button class="mdl-button mdl-js-button mdl-color-text--primary-contrast
                 mdl-button--colored mdl-button--raised"
          show={ !isAuthenticated }>
    <a href={ loginUrl }>Log in with Twitch</a>
  </button>


  <!-- style -->
  <style scoped>
    a {
      text-decoration: none;
      color: unset;
    }
  </style>


  <!-- logic -->
  <script>
    import TwitchAPI from 'api/twitch.js'

    this.isAuthenticated = TwitchAPI.isAuthenticated()
    this.loginUrl = TwitchAPI.OAuth.authorizeUrl()

    let redirectHash = new URL(TWITCH_APP_REDIRECT_URI).hash

    riot.route(`/${redirectHash}..`, () => {
      let code = riot.route.query().code

      TwitchAPI.OAuth.token(code)
        .then(data => {
          TwitchAPI.saveToken(data.access_token)
          this.update({ isAuthenticated: true })
          riot.route('/following')
        })
    })
  </script>

</twitch-login-button>
