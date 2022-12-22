#!/bin/sh

set -e

THIS_DIR="$(dirname "$(realpath "$0")")"
USER="${USER:-user}"
NIX_CONF="http2 = false
trusted-users = root $USER
extra-substituters = https://cache.nixos.org https://hydra.iohk.io
trusted-public-keys = cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY= hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ=
"

docker run -it --rm \
  -v "$THIS_DIR/..:/app" \
  -v "nix-$USER:/nix" \
  -v "nix-home-$USER:/home/$USER" \
  --security-opt seccomp=unconfined \
  -w "/app" nixos/nix:2.3.12 \
  sh -c "
  adduser $USER -D &&
  echo \"$NIX_CONF\" >> /etc/nix/nix.conf &&
  (nix-daemon &) &&
  sleep 1 &&
  su $USER -c \"NIX_REMOTE=daemon \
    nix-shell \
    ./nix/shell.nix \
    --pure --show-trace -v \
    --arg mini true\"
  "
