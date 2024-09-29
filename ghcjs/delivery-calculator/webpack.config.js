const webpack = require("webpack");

module.exports = {
  entry : {
    miso_widgets : "../miso-functora/js/main.js",
    wasm : "./static/wasm.js",
  },
  output : {
    path : __dirname + "/dist/latest",
    filename : "[name].js",
    // Export itself to a global variable
    libraryTarget : "var",
    library : "h$[name]",
  },
  mode : "production",
  optimization : {
    minimize : false,
  },
  plugins :
          [
            new webpack.IgnorePlugin({resourceRegExp : /^node:timers$/}),
            new webpack.optimize.LimitChunkCountPlugin({
              maxChunks : 1,
            }),
          ],
};
