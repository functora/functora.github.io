let
  pkgs = import ./../../../nix/nixpkgs.nix;
  unst = import ./../../../nix/nixpkgs-unstable.nix;
  olds = import (fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/a94ea3486da532f88e87e698b2f0e6b87aea321e.tar.gz";
    sha256 = "1h9x7zcfxhmakfn5azaxg7wsakz3g47cwfgpqp6kc96h0by8r5k8";
  }) {};
in
  {
    ai ? false,
    formatter ? "ormolu",
    vimBackground ? "light",
    vimColorScheme ? "PaperColor", # "edge",
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
      pretty = pkgs.writeShellApplication {
        name = "prettier";
        text = ''
          ${
            pkgs.nodePackages.prettier
          }/bin/prettier --plugin ${
            pkgs.nodePackages.prettier-plugin-toml
          }/lib/node_modules/prettier-plugin-toml/lib/index.js "$@"
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
      vi = unst.neovim.override {
        viAlias = true;
        vimAlias = true;
        configure = {
          customRC =
            ''
              set runtimepath+=${vi-src}
              let $PATH.=':${silver-searcher}/bin:${nodejs}/bin:${less}/bin:${lesspipeWrapper}/bin:${python311Packages.grip}/bin:${xdg-utils}/bin:${git}/bin:${jre8}/bin:${stylua}/bin:${sleek}/bin:${pretty}/bin:${html-tidy}/bin'
              let $SHELL='/run/current-system/sw/bin/bash'
              let g:vimBackground = '${vimBackground}'
              let g:vimColorScheme = '${vimColorScheme}'
              let g:languagetool_jar='${olds.languagetool}/share/languagetool-commandline.jar'
              source ${vi-src}/vimrc.vim
              luafile ${vi-src}/vimrc.lua
              try
                source ~/.vi/vimrc.vim
              catch
              endtry
            ''
            + (getAttr formatter formatter-registry);
          packages.vim-functora = with pkgs.vimPlugins; {
            start =
              [
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
                psc-ide-vim
                purescript-vim
                neoformat
                vim-commentary
                render-markdown-nvim
                rust-vim
                coc-snippets
                unst.vimPlugins.coc-rust-analyzer
                #
                # Productivity
                #
                coc-nvim
                sideways-vim
                vim-LanguageTool
              ]
              ++ (
                if ai
                then [
                  unst.vimPlugins.minuet-ai-nvim
                  unst.vimPlugins.codecompanion-nvim
                  unst.vimPlugins.codecompanion-spinner-nvim
                ]
                else []
              );
            opt = [
            ];
          };
        };
      };
    in
      vi.overrideAttrs (next: prev: {
        nativeBuildInputs =
          (
            if builtins.hasAttr "nativeBuildInputs" prev
            then prev.nativeBuildInputs
            else []
          )
          ++ [
            pkgs.makeShellWrapper
          ];
        postInstall =
          (
            if builtins.hasAttr "postInstall" prev
            then prev.postInstall
            else ""
          )
          + ''
            wrapProgram $out/bin/vi \
              --set SHELL /run/current-system/sw/bin/bash
          '';
      })
