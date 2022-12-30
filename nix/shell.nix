(import ./default.nix).shellFor {
  exactDeps = true;
  withHoogle = true;
  tools = {
    cabal = "latest";
    hlint = "latest";
    haskell-language-server = "latest";
  };
}
