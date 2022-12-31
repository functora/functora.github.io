(import ./default.nix).shellFor {
  exactDeps = true;
  withHoogle = true;
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "1.8.0.0";
  };
}
