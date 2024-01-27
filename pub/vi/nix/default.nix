let
  pkgs = import ./../../../nix/nixpkgs.nix;
in
  {
    mini ? true,
    formatter ? "ormolu",
    vimBackground ? "light",
    vimColorScheme ? "everforest",
  }:
    with pkgs;
    with builtins;
    with lib.lists; let
      ignore-patterns = ''
        .git
        .gitignore
        *.nix
        *.sh
        *.md
        LICENSE
        result
      '';
      formatter-registry = {
        ormolu = ''
          let g:brittany_on_save = 0
          let g:ormolu_disable = 0
        '';
        brittany = ''
          let g:brittany_on_save = 1
          let g:ormolu_disable = 1
        '';
        none = ''
          let g:brittany_on_save = 0
          let g:ormolu_disable = 1
        '';
      };
      lesspipeWrapper = writeShellScriptBin "lesspipe" "${lesspipe}/bin/lesspipe.sh";
      vi-src = stdenv.mkDerivation {
        name = "vi-src";
        src = nix-gitignore.gitignoreSourcePure ignore-patterns ./..;
        dontBuild = true;
        installPhase = ''
          mkdir -p $out/
          cp -R ./ $out/
        '';
      };
      vi = neovim.override {
        viAlias = true;
        vimAlias = true;
        configure = {
          customRC =
            ''
              set runtimepath+=${vi-src}
              let $PATH.=':${silver-searcher}/bin:${nodejs}/bin:${less}/bin:${lesspipeWrapper}/bin:${python311Packages.grip}/bin:${xdg_utils}/bin:${git}/bin:${jre8}/bin'
              let g:vimBackground = '${vimBackground}'
              let g:vimColorScheme = '${vimColorScheme}'
              let g:languagetool_jar='${languagetool}/share/languagetool-commandline.jar'
              source ${vi-src}/vimrc.vim
              try
                source ~/.vi/vimrc.vim
              catch
              endtry
            ''
            + (getAttr formatter formatter-registry);
          packages.vim-functora = with pkgs.vimPlugins; {
            start = [
              #
              # Interface
              #
              ack-vim
              ctrlp-vim
              vim-fugitive
              vim-gitgutter
              lightline-vim
              vim-togglelist
              papercolor-theme
              vim-better-whitespace
              catppuccin-nvim
              everforest
              edge
              #
              # Programming
              #
              haskell-vim
              hlint-refactor-vim
              vim-nix
              dhall-vim
              psc-ide-vim
              purescript-vim
              vim-elixir
              gleam-vim
              neoformat
              vim-terraform
              #
              # Productivity
              #
              coc-nvim
              sideways-vim
              vim-LanguageTool
            ];
            opt = [
            ];
          };
        };
      };
    in
      if mini
      then vi
      else {
        #
        # Vi
        #
        inherit vi;
        #
        # Haskell
        #
        ghc = haskell.compiler.ghc902;
        stack = haskellPackages.stack;
        cabal = cabal-install;
        hlint = haskellPackages.hlint;
        hoogle = haskellPackages.hoogle;
        apply-refact = haskellPackages.apply-refact;
        hspec-discover = haskellPackages.hspec-discover;
        implicit-hie = haskellPackages.implicit-hie;
        ormolu = haskellPackages.ormolu;
        brittany = haskellPackages.brittany;
        inherit zlib haskell-language-server cabal2nix ghcid;
        #
        # Dhall
        #
        inherit dhall dhall-json;
        #
        # Misc
        #
        inherit nix niv git curl;
      }
