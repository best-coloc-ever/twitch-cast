<notice>

  <h1 if={ visible }>{ text }</h1>

  <style scoped>
    h1 {
      color: white;
      text-align: center;
    }
  </style>

  <script>
    this.visible = false;
    this.text = '';

    show(text) {
      this.visible = true;
      this.text = text;
      this.update();
    }

    hide() {
      this.visible = false;
      this.update();
    }
  </script>

</notice>
