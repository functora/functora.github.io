module.exports = {
    entry: './js/index.js',
    output: {
        path: __dirname + '/js',
        filename: 'index.compiled.js',
        // Export itself to a global variable "h$bitcoin_hash"
        libraryTarget: "var",
        library: "h$bitcoin_hash"
    }
};
