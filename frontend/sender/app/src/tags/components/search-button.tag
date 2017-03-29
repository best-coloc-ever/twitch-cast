<search-button>

  <!-- layout -->
  <form onsubmit={ doSearch }>
    <div class="mdl-textfield mdl-js-textfield mdl-textfield--expandable search"
         ref="mdl-textfield">
      <label class="mdl-button mdl-js-button mdl-button--icon" for="search-input">
        <i class="material-icons mdl-color-text--primary-contrast"">search</i>
      </label>
      <div class="mdl-textfield__expandable-holder">
        <input class="mdl-textfield__input mdl-color-text--primary-contrast"
               type="text" placeholder="Search"
               id="search-input" ref="search-input"
               onkeyup={ onInputKeyUp }>
        <label class="mdl-textfield__label"></label>
      </div>
    </div>
  </form>


  <!-- style -->
  <style scoped>

  </style>

  <!-- logic -->
  <script>
    import { routeLinks } from 'routing/routes.js'

    this.doSearch = event => {
      let input = this.refs['search-input'],
          textField = this.refs['mdl-textfield'],
          query = input.value

      // Reseting the textfield state...
      input.value = ''
      textField.classList.remove(
        textField.MaterialTextfield.CssClasses_.IS_FOCUSED,
        textField.MaterialTextfield.CssClasses_.IS_DIRTY,
      )

      riot.route(routeLinks.search(query))

      event.preventDefault()
    }
  </script>

</search-button>
