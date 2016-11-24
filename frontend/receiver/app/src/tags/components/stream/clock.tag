<clock>

  <div>{ hours }{ separator() }{ minutes }</div>

  <script>
    import { zpad } from 'utils/formatting.js'

    let refreshTimer = null
    let blinkerSwitch = true

    this.separator = () => this.blinkerSwitch ? ':' : ' '

    this.refresh = () => {
      let now = new Date()

      this.hours = zpad(now.getHours(), 2)
      this.minutes = zpad(now.getMinutes(), 2)
      blinkerSwitch = !blinkerSwitch

      this.update()
    }

    this.on('mount', () => {
      this.refresh()
      refreshTimer = setInterval(this.refresh, 1000)
    })

    this.on('unmount', () => {
      clearInterval(refreshTimer)
    })
  </script>

</clock>
