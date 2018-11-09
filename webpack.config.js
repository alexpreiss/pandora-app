const path = require('path')
const HtmlWebpackPlugin = require('html-webpack-plugin')
const CopyWebpackPlugin = require('copy-webpack-plugin')

const resolve = subPath => path.resolve(__dirname, subPath)

module.exports = {
  mode: 'development',
  devServer: {
    proxy: {
      '/api': {
        target: 'http://localhost:3000/proxy',
        pathRewrite: { '^/api': '' }
      }
    }
  },
  entry: {
    index: resolve('./src/index.js')
  },
  context: resolve('.'),
  output: { path: resolve('./build') },
  module: {
    strictExportPresence: true,
    noParse: /\.elm$/,
    rules: [
      {
        test: /\.elm$/,
        loader: 'elm-webpack-loader'
      },
      {
        test: /\.css/,
        use: [
          'style-loader',
          'css-loader'
        ]
      }
    ]
  },
  plugins: [
    new HtmlWebpackPlugin({
      inject: true,
      chunks: [ 'index' ],
      filename: 'index.html',
      template: resolve('./src/template.html')
    }),
    new CopyWebpackPlugin([ { from: 'src/main.js', to: 'main.js' }, "assets" ])
  ]
}
