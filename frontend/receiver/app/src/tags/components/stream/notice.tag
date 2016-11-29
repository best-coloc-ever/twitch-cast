<notice>

  <h3 show={ visible }>{ text }</h3>

  <style scoped>
    h3 {
      color: white;
      text-align: center;
    }
  </style>

  <script>
    this.visible = false
    this.text = ''

    this.show = (text) => this.update({ visible: true, text: text })
    this.hide = ()     => this.update({ visible: false })
  </script>

</notice>
