<!-- Was forced to make this proxy tag for some reason... -->
<data-card>

  <tag ref="target"></tag>

  <script>
    this.on('mount', () => {
      riot.mount(this.refs.target, opts.cardTag, { data: opts.forwardData })
    })
  </script>

</data-card>

<card-list-view>

  <!-- layout -->
  <h5 align="center" show={ !rows.length && !hasMoreData }>No results 😢</h5>

  <div class="mdl-grid data-grid" each={ row in rows }>
    <div class={ cardCellClasses }
         each={ data in row }>
      <data-card
        card-tag={ parent.config.cardTag }
        forward-data={ data }>
      </data-card>
    </div>
  </div>

  <div class="bottom-wrapper">
    <div
      class="mdl-progress mdl-js-progress mdl-progress__indeterminate progress-bar"
      show={ loading }>
    </div>
    <button
      class="mdl-button mdl-js-button mdl-button--raised mdl-button--colored"
      onclick={ fetchNext }
      show={ !loading && hasMoreData }>
      Load more
    </button>
  </div>


  <!-- style -->
  <style scoped>
    .data-grid {
      padding: 0;
    }

    .bottom-wrapper {
      text-align: center;
      padding-top: 15px;
      padding-bottom: 15px;
    }

    .progress-bar {
      margin: 0 auto;
    }
  </style>


  <!-- logic -->
  <script>
    import { leastCommonMultiple } from 'utils/math.js'

    const rowCount = 8

    let config = opts.config
    let offset = 0
    let fetchCount = Math.max(...Object.values(config.rowSizes)) * rowCount
    let rowSize = leastCommonMultiple(...Object.values(config.rowSizes))

    this.config = config

    this.rows = []
    this.loading = false
    this.hasMoreData = true

    this.cardCellClasses = [
      'mdl-cell',
      `mdl-cell--${12 / config.rowSizes.desktop}-col-desktop`,
      `mdl-cell--${8 / config.rowSizes.tablet}-col-tablet`,
      `mdl-cell--${4 / config.rowSizes.phone}-col-phone`,
    ].join(' ')

    this.addData = data => {
      let i = 0
      // Make sure we fill out the last row if any
      if (this.rows.length) {
        let lastRowIdx = this.rows.length - 1
        let lastRow = this.rows[lastRowIdx]

        while (lastRow.length < rowSize && i < data.length)
          lastRow.push(data[i++])
      }
      // Split the remaining data
      while (i < data.length) {
        this.rows.push(data.slice(i, i + rowSize))
        i += rowSize
      }
    }

    this.fetchNext = () => {
      this.update({ loading: true })

      let params = {
        offset: offset,
        limit: fetchCount,
      }

      config.fetchLogic(params)
        .then(rawData => {
          let data = config.dataFilter(rawData)
          this.addData(data)

          offset += data.length
          this.update({
            loading: false,
            hasMoreData: (offset < rawData._total)
          })
        })
    }

    this.on('mount', () => {
      componentHandler.upgradeElements(this.root)

      this.fetchNext()
    })
  </script>

</card-list-view>
