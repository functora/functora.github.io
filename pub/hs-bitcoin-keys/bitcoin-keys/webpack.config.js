module.exports = {
    entry: './js/index.js',
    output: {
        path: __dirname + '/js',
        filename: 'index.compiled.js',
        // Export itself to a global variable
        libraryTarget: "var",
        library: "h$bitcoin_keys"
    }
};
