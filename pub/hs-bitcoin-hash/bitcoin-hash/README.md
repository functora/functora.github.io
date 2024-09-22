# bitcoin-hash

Bitcoin hashing primitives.

This library builds on GHC and GHCJS.


## Developing in GHCJS

* `cabal --ghcjs build` will build the Haskell `bitcoin-hash` library with the
  JavaScript dependencies already compiled at `js/index.compiled.js`.

* If the `js/index.js` file changes, run `npx webpack` to regenerate
  `js/index.compiled.js` and commit both files to the repository.

* If the `package.json` file changes, run `npm install -D` to get new JS
  dependencies, and then run `npx webpack` as above.
