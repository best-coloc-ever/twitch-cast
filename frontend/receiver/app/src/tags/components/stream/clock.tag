<clock>

  <div>{ hours }{ separator() }{ minutes }</div>

  <script>
    this.blinkerSwitch = true

    function zpad(what, n) {
      what = what + '' // Make it a string

      if (what.length >= n)
        return what

      return new Array(n - what.length + 1).join('0') + what
    }

    this.separator = () => this.blinkerSwitch ? ':' : ' '

    this.refresh = () => {
      let now = new Date()

      this.hours = zpad(now.getHours(), 2)
      this.minutes = zpad(now.getMinutes(), 2)
      this.blinkerSwitch = !this.blinkerSwitch

      this.update()
    }

    this.on('mount', () => {
      this.refresh()
      setInterval(this.refresh, 1000)
    })
  </script>

</clock>
