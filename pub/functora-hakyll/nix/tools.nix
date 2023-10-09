with (import ./../../../nix/project.nix);
with pkgs; let
  cabal = "${pkgs.cabal-install}/bin/cabal";
  pkgDir = builtins.toString ./..;
  hakyllBuild = writeShellScriptBin "hakyll-build" ''
    (
      cd ${pkgDir} && \
      ${cabal} new-run site clean && \
      ${cabal} new-run site rebuild && \
      sleep 1 && \
      (
        cd ${pkgDir}/docs && \
        ${htmldoc}/bin/htmldoc \
          -f cv.pdf \
          --webpage --header "..." --footer ".1." \
          ./formal.html
      )
    )
  '';
  hakyllWatch = writeShellScriptBin "hakyll-watch" ''
    (
      cd ${pkgDir} && \
      ${hakyllBuild}/bin/hakyll-build && \
      ${cabal} new-run site watch
    )
  '';
in [
  hakyllBuild
  hakyllWatch
]
