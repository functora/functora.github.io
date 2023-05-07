#!/bin/bash

set -eo pipefail

#
# NOTE : please install nix before invoking this script
#

TERM=xterm-color
DOTFILES_SOURCE_DIR="$(dirname "$(readlink -m "$0")")"

log () {
  echo "$1 ==> $2"
}

log_error () {
  log "error" "$1"
}

log_success () {
  log "success" "$1"
}

log_bundle () {
  log "bundle" "$1"
}

log_updating () {
  log "updating" "$1"
}

log_already_exists () {
  log "already exists" "$1"
}

log_creating () {
  log "creating" "$1"
}

log_installing () {
  log "installing" "$1"
}

log_already_installed () {
  log "already installed" "$1"
}

lazy_copy () {
  local DOTFILES_SOURCE
  local DOTFILES_TARGET
  local DOTFILES_TARGET_DIR
  DOTFILES_SOURCE="$DOTFILES_SOURCE_DIR/$1"
  DOTFILES_TARGET="$2"
  DOTFILES_TARGET_DIR="$(dirname "$(readlink -m "$2")")"
  if [ -d "$DOTFILES_TARGET_DIR" ]; then
    log_already_exists "$DOTFILES_TARGET_DIR"
  else
    log_creating "$DOTFILES_TARGET_DIR"
    mkdir -p "$DOTFILES_TARGET_DIR"
  fi
  if [ -f "$DOTFILES_TARGET" ]; then
    log_already_exists "$DOTFILES_TARGET"
  else
    log_creating "$DOTFILES_TARGET"
    cp "$DOTFILES_SOURCE" "$DOTFILES_TARGET"
  fi
}

lazy_append () {
  grep -q "$1" "$2" || echo "$1" >> "$2"
}

lazy_install () {
  # 1st arg = executable name (required)
  # 2nd arg = pkg name (optional)
  # 3rd arg = installation command (optional)
  local PKG
  PKG="$([ -z "$2" ] && echo "$1" || echo "$2")"
  (command -v "$1" > /dev/null || \
    nix-env -q | grep "$1" > /dev/null || \
    nix-env -q | grep "$PKG" > /dev/null ) && \
    log_already_installed "$PKG" || \
    strict_install "$PKG" "$3"
}

strict_install () {
  log_installing "$1"
  if [ -z "$2" ]; then
    nix-env -iA "nixpkgs.$1"
  else
    eval "$2"
  fi
}

if [[ $UID == 0 ]]; then
  log_error "please don't run this script with sudo!"
  exit 1
fi

log_updating "apt-get"
sudo apt-get update -y

(
  log_bundle "i3wm"
  sudo apt-get install -y i3
  lazy_install "playerctl"
  lazy_copy i3wm-config ~/.config/i3/config
  lazy_copy i3wm-status-config ~/.i3status.conf
)

(
  log_bundle "termite"
  lazy_copy termite-config ~/.config/termite/config
  lazy_install "termite" "termite" "nix-env -iA nixpkgs.termite && tic -x $DOTFILES_SOURCE_DIR/termite.terminfo"
  sudo apt-get install -y language-pack-ru ttf-ancient-fonts
)

(
  log_bundle "apps"
  lazy_install "git"
  lazy_install "brave"
)

sudo apt-get autoremove -y

log_success "installation finished"
