{crossSystem ? null}: let
  # Read in the Niv sources
  sources = import ./sources.nix {};
  # If ./nix/sources.nix file is not found run:
  #   niv init
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Fetch the haskell.nix commit we have pinned with Niv
  haskellNix = import sources.haskellNix {};
  # If haskellNix is not found run:
  #   niv add input-output-hk/haskell.nix -n haskellNix

  # Import nixpkgs and pass the haskell.nix provided nixpkgsArgs
  prev =
    import
    # haskell.nix provides access to the nixpkgs pins which are used by our CI,
    # hence you will be more likely to get cache hits when using these.
    # But you can also just use your own, e.g. '<nixpkgs>'.
    haskellNix.sources.nixpkgs-unstable
    # These arguments passed to nixpkgs, include some patches and also
    # the haskell.nix functionality itself as an overlay.
    haskellNix.nixpkgsArgs;

  # Apply crossSystem if needed
  next =
    if crossSystem == null
    then pkgs
    else
      import haskellNix.sources.nixpkgs-unstable (
        haskellNix.nixpkgsArgs
        // {
          crossSystem = pkgs.lib.systems.examples."${crossSystem}";
        }
      );

  # Nixpkgs overlay to fix haskell tdlib bindings.
  pkgs = prev.extend (
    _: x: {tdjson = x.tdlib;}
  );
in
  next.haskell-nix.project {
    # 'cleanGit' cleans a source directory based on the files known by git
    src = pkgs.haskell-nix.haskellLib.cleanGit {
      name = "functora";
      src = ./..;
    };
    # Specify the GHC version to use.
    #
    # NOTE : Fallback from 948 to 928 for aarch64 cross support.
    #
    compiler-nix-name = "ghc928"; # Not required for `stack.yaml` based projects.
    projectFileName = "cabal.project";
  }
