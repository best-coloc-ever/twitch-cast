<home-view>

  <!-- layout -->
  <div></div>


  <!-- style -->
  <style scoped>

  </style>


  <!-- logic -->
  <script>
    this.on('mount', () => {
      riot.route('/channels', '', true)
    })
  </script>

</home-view>
