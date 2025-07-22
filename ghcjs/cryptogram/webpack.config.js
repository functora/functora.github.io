const webpack = require("webpack");
const { EsbuildPlugin } = require("esbuild-loader");

module.exports = {
  entry: {
    miso_functora: "../miso-functora/js/main.js",
    wasm: "./static/wasm.js",
  },
  output: {
    path: __dirname + "/dist/latest",
    filename: "[name].js",
    libraryTarget: "var",
    library: "h$[name]",
  },
  mode: "production",
  optimization: {
    minimize: true,
    minimizer: [
      new EsbuildPlugin({
        target: "esnext",
        minify: false,
        minifySyntax: true,
        minifyWhitespace: true,
        minifyIdentifiers: false,
      }),
    ],
  },
  plugins: [
    new webpack.IgnorePlugin({ resourceRegExp: /^node:timers$/ }),
    new webpack.optimize.LimitChunkCountPlugin({
      maxChunks: 1,
    }),
  ],
};
