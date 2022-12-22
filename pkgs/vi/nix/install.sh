#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
VIM_BACKGROUND="${VIM_BACKGROUND:-light}"
VIM_COLOR_SCHEME="${VIM_COLOR_SCHEME:-PaperColor}"

MINI="true"

if [ -z "$*" ]; then
  echo "vi ==> using defaults"
else
  for ARG in "$@"; do
    case $ARG in
      mini)
        MINI="true"
        shift
        ;;
      maxi)
        MINI="false"
        shift
        ;;
      *)
        echo "vi ==> unrecognized ARG $ARG"
        exit 1
        ;;
    esac
  done
fi

echo "vi ==> installing with MINI=$MINI"

nix-env -i \
  -f "$THIS_DIR/default.nix" \
  --show-trace -v \
  --arg mini "$MINI" \
  --argstr vimBackground $VIM_BACKGROUND \
  --argstr vimColorScheme $VIM_COLOR_SCHEME \
  --option extra-substituters "https://cache.nixos.org https://hydra.iohk.io" \
  --option trusted-public-keys "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="

echo "vi ==> successfully installed with MINI=$MINI"
