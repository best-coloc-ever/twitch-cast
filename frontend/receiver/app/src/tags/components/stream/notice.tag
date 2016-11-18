<notice>

  <h3 show={ visible }>{ text }</h3>

  <style scoped>
    h3 {
      color: white;
      text-align: center;
    }
  </style>

  <script>
    import { PlayerEvent } from 'player/events.js'

    this.visible = false
    this.text = ''

    this.show = (text) => {
      this.visible = true
      this.text = text
      this.update()
    }

    this.hide = () => {
      this.visible = false
      this.update()
    }

    this.on('mount', () => {
      opts.player.on(PlayerEvent.AutoPaused, isPaused => {
        if (isPaused) this.show('Buffering...')
        else          this.hide()
      })

      opts.player.on(PlayerEvent.HostError, error => {
        this.show(error)
      })
    })
  </script>

</notice>
