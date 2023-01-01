with (import ./project.nix);
shellFor {
  exactDeps = true;
  withHoogle = false;
  #buildInputs = [
  #  pkgs.haskell-language-server
  #];
  #tools = {
  #  cabal = "latest";
  #  hlint = "latest";
  #};
}
