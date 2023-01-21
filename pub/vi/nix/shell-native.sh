#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
VIM_BACKGROUND="${VIM_BACKGROUND:-light}"
VIM_COLOR_SCHEME="${VIM_COLOR_SCHEME:-PaperColor}"

nix-shell \
  "$THIS_DIR/shell.nix" \
  --pure --show-trace -v \
  --arg mini true \
  --argstr vimBackground $VIM_BACKGROUND \
  --argstr vimColorScheme $VIM_COLOR_SCHEME \
  --option extra-substituters "https://cache.nixos.org https://hydra.iohk.io" \
  --option trusted-public-keys "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
