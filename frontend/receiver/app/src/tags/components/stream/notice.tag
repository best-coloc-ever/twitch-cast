<notice>

  <h1 show={ visible }>{ text }</h1>

  <style scoped>
    h1 {
      color: white;
      text-align: center;
    }
  </style>

  <script>
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
  </script>

</notice>
