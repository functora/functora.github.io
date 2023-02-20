#!/bin/sh

set -e

CUR="$(dirname "$(realpath "$0")")"
PKG="$1"

if [ $# -ne 1 ]; then
  echo "==> exactly one package name argument is required"
  exit 1
fi

echo "==> installing $PKG"
nix-env -f $CUR/project.nix -iAP $PKG.components.exes.$PKG-exe
echo "==> finished $PKG installation"
