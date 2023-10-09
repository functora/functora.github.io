with (import ./../../../nix/project.nix);
with pkgs; let
  cabal = "${pkgs.cabal-install}/bin/cabal";
  pkgDir = builtins.toString ./..;
  repoDir = builtins.toString ./../../..;
  hakyllBuild = writeShellScriptBin "hakyll-build" ''
    (
      cd ${pkgDir} && \
      ${cabal} new-run site clean && \
      sleep 1 && \
      ${cabal} new-run site rebuild && \
      sleep 2 && \
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
  hakyllPublish = writeShellScriptBin "hakyll-publish" ''
    ${hakyllBuild}/bin/hakyll-build && \
    rm -rf ${repoDir}/docs && \
    cp -R ${pkgDir}/docs/ ${repoDir}/docs
  '';
in [
  hakyllBuild
  hakyllWatch
  hakyllPublish
]
