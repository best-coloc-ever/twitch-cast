var webpack = require('webpack')
var CopyWebpackPlugin = require('copy-webpack-plugin');

module.exports = {
  entry: {
    app: './src/index.js',
    vendor: './src/vendor.js'
  },

  output: {
    path: '/dist',
    filename: 'bundle.js'
  },

  module: {
    preLoaders: [
      // Riot
      {
        test: /\.tag$/,
        include: /src/,
        loader: 'riotjs',
        query: { type: 'none' }
      },
    ],
    loaders: [
      {
        test: /\.js$|\.tag$/,
        include: /src/,
        loader: 'babel',
        query: { presets: 'es2015-riot' }
      }
    ],
  },

  babel: {
    presets: ['es2015'],
  },

  plugins: [
    new webpack.ProvidePlugin({ riot: 'riot' }),
    new CopyWebpackPlugin([
      { from: 'index.html' },
      { from: 'styles.css' }
    ]),
    new webpack.optimize.CommonsChunkPlugin(
      /* chunkName= */'vendor',
      /* filename= */'vendor.bundle.js'
    ),
  ]
};
