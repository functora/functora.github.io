const webpack = require("webpack");
const TerserPlugin = require("terser-webpack-plugin");
module.exports = {
  entry: "./js/main.js",
  output: {
    path: __dirname + "/js",
    filename: "main.min.js",
    // Export itself to a global variable
    libraryTarget: "var",
    library: "h$miso_capa",
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
