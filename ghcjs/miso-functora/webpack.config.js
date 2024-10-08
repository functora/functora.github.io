const webpack = require("webpack");
const TerserPlugin = require("terser-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CssMinimizerPlugin = require("css-minimizer-webpack-plugin");
const RemoveEmptyScriptsPlugin = require("webpack-remove-empty-scripts");
const js = {
  entry: "./js/main.js",
  output: {
    path: __dirname + "/js",
    filename: "main.min.js",
    // Export itself to a global variable
    libraryTarget: "var",
    library: "h$miso_functora",
  },
  optimization: {
    minimize: true,
    minimizer: [
      new TerserPlugin({
        minify: TerserPlugin.swcMinify,
        // `terserOptions` options will be passed to `swc` (`@swc/core`)
        // Link to options - https://swc.rs/docs/config-js-minify
        terserOptions: {},
      }),
    ],
  },
  plugins: [
    new webpack.optimize.LimitChunkCountPlugin({
      maxChunks: 1,
    }),
  ],
};
const css = {
  entry: {
    paper: "./css/paper.css",
  },
  output: {
    path: __dirname + "/dist/css",
    clean: true,
  },
  mode: "production",
  optimization: {
    minimize: true,
    minimizer: [new CssMinimizerPlugin()],
  },
  plugins: [
    new MiniCssExtractPlugin({
      filename: "[name].min.css",
    }),
    new webpack.optimize.LimitChunkCountPlugin({
      maxChunks: 1,
    }),
    new RemoveEmptyScriptsPlugin(),
  ],
  module: {
    rules: [
      {
        test: /\.css$/,
        use: [
          MiniCssExtractPlugin.loader,
          { loader: "css-loader", options: { importLoaders: 1 } },
          {
            loader: "postcss-loader",
            options: { postcssOptions: { plugins: ["postcss-import-url"] } },
          },
        ],
      },
    ],
  },
  experiments: {
    buildHttp: {
      allowedUris: [/^(http|https):\/\//],
      frozen: false,
    },
  },
};

module.exports = [js, css];
