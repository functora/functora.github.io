const webpack = require("webpack");
module.exports = {
  entry: "./js/main.js",
  output: {
    path: __dirname + "/js",
    filename: "main.min.js",
    // Export itself to a global variable
    libraryTarget: "var",
    library: "h$miso_widgets",
  },
  plugins: [
    new webpack.optimize.LimitChunkCountPlugin({
      maxChunks: 1,
    }),
  ],
};
