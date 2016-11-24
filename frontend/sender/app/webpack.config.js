var webpack = require('webpack')
var path = require('path');
var CopyWebpackPlugin = require('copy-webpack-plugin')

module.exports = {
  entry: {
    app: './src/index.js',
    vendor: './src/vendor.js'
  },

  output: {
    path: '/dist',
    filename: 'bundle.js'
  },

  resolve: {
    root: [
      path.resolve('./src')
    ]
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
    new webpack.ProvidePlugin({
      riot: 'riot'
    }),
    new CopyWebpackPlugin([
      { from: 'img', to: 'img' },
      { from: 'index.html' },
      { from: 'styles.css' }
    ]),
    new webpack.optimize.CommonsChunkPlugin(
      /* chunkName= */'vendor',
      /* filename= */'vendor.bundle.js'
    ),
    new webpack.DefinePlugin({
      TWITCH_CLIENT_ID:        JSON.stringify(process.env.TWITCH_CLIENT_ID),
      TWITCH_CLIENT_SECRET:    JSON.stringify(process.env.TWITCH_CLIENT_SECRET),
      TWITCH_APP_REDIRECT_URI: JSON.stringify(process.env.TWITCH_APP_REDIRECT_URI),
      CHROMECAST_APP_ID:       JSON.stringify(process.env.CHROMECAST_APP_ID)
    })
  ]
};
