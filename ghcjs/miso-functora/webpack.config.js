const webpack = require("webpack");
const TerserPlugin = require("terser-webpack-plugin");
const MiniCssExtractPlugin = require("mini-css-extract-plugin");
const CssMinimizerPlugin = require("css-minimizer-webpack-plugin");
const RemoveEmptyScriptsPlugin = require("webpack-remove-empty-scripts");
const path = require("path");
const fs = require("fs");

const jsConfig = {
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

function cssEntry(cssDir) {
  const input = __dirname + "/lib/" + cssDir;
  return fs
    .readdirSync(input)
    .filter((file) => file.endsWith(".css"))
    .filter((file) =>
      [
        "ads-gazette.css",
        "ads-notebook.css",
        "ads-medium.css",
        "scooter.css",
        "propeller.css",
        "motherplate.css",
        "boot-paper.css",
        "boot-readable.css",
        "flat-ui.css",
        "pandoc-scholar.css",
        "tui.css",
        "latex.css",
        "ok.css",
        "hello.css",
        "minimal.css",
        "siimple.css",
        "missing-style.css",
        "semantic-ui.css",
        "w3c-traditional.css",
        "primer.css",
        "yamb.css",
      ].every((bad) => file != bad),
    )
    .map((file) => [path.basename(file, ".css"), path.join(input, file)])
    .reduce(
      (entries, [name, filePath]) => ({ ...entries, [name]: filePath }),
      {},
    );
}

function cssConfig(cssDir) {
  return {
    entry: cssEntry(cssDir),
    output: {
      path: __dirname + "/dist/" + cssDir,
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
}

module.exports = [jsConfig, cssConfig("themes"), cssConfig("miso-functora")];
