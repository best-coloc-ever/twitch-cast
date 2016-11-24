<search-button>

  <!-- layout -->
  <form onsubmit={ doSearch }>
    <div class="mdl-textfield mdl-js-textfield mdl-textfield--expandable search"
         name="mdl-textfield">
      <label class="mdl-button mdl-js-button mdl-button--icon" for="search-input">
        <i class="material-icons mdl-color-text--primary-contrast"">search</i>
      </label>
      <div class="mdl-textfield__expandable-holder">
        <input class="mdl-textfield__input mdl-color-text--primary-contrast"
               type="text" id="search-input" placeholder="Search"
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

    let input = this['search-input']
    let textField = this['mdl-textfield']

    this.doSearch = _ => {
      let query = input.value
      input.value = ''

      // Reseting the textfield state...
      textField.classList.remove(
        textField.MaterialTextfield.CssClasses_.IS_FOCUSED,
        textField.MaterialTextfield.CssClasses_.IS_DIRTY,
      )

      riot.route(routeLinks.search(query))

      return false
    }
  </script>

</search-button>
