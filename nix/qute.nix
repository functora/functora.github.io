{pkgs ? import <nixpkgs> {}}: let
  nixpak = import ./nixpak.nix;
  mkNixPak = nixpak.lib.nixpak {
    inherit (pkgs) lib;
    inherit pkgs;
  };
  app = pkgs.writeShellApplication {
    name = "qute";
    text = ''
      ${pkgs.qutebrowser}/bin/qutebrowser \
        -C ${../cfg/qutebrowser.py} "$@"
    '';
  };
  ytb = pkgs.writeTextFile {
    name = "ytb.js";
    text = ''
      // ==UserScript==
      // @name         Youtube Enhancements
      // @match        *://*.youtube.com/*
      // ==/UserScript==

      (function() {
          'use strict';

          function skipAds() {
              const skipBtn = document.querySelector('.videoAdUiSkipButton, .ytp-ad-skip-button-modern, .ytp-skip-ad-button');
              // if (skipBtn) skipBtn.click();
              const adVideo = document.querySelector('.ad-showing .video-stream');
              adVideo.muted = true;
              adVideo.hidden = true;
              adVideo.playbackRate = 2;
              // if (adVideo && adVideo.duration > 0 && adVideo.currentTime < adVideo.duration) {
              //     adVideo.currentTime = adVideo.duration;
              // }
          }

          function removeSponsored() {
              document.querySelectorAll('ytd-in-feed-ad-layout-renderer').forEach(el => el.remove());
              document.querySelectorAll('ytd-engagement-panel-section-list-renderer[target-id="engagement-panel-ads"]').forEach(el => el.remove());
          }

          function observer() {
              skipAds();
              removeSponsored();
          }

          window.addEventListener('load', observer);
          new MutationObserver(observer).observe(document.body, { childList: true, subtree: true });
      })();
    '';
  };
  sandbox = mkNixPak {
    config = {sloth, ...}: {
      app.package = app;
      gpu.enable = true;
      gpu.provider = "bundle";
      fonts.enable = true;
      locale.enable = true;
      etc.sslCertificates.enable = true;
      bubblewrap = {
        network = true;
        sockets.pulse = true;
        sockets.wayland = true;
        bind.ro = [
          [
            (toString ytb)
            (
              sloth.concat'
              sloth.homeDir
              "/.config/qutebrowser/greasemonkey/ytb.js"
            )
          ]
        ];
        bind.rw = [
          [
            (sloth.mkdir (sloth.concat' sloth.homeDir "/qute"))
            sloth.homeDir
          ]
        ];
        tmpfs = [
          "/tmp"
        ];
      };
    };
  };
in
  sandbox.config.env
