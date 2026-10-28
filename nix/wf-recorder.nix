{pkgs ? import <nixpkgs> {}}:
import ./bubble.nix {
  inherit pkgs;
  name = "wf-recorder";
  text = ''${pkgs.wf-recorder}/bin/wf-recorder -a -c h264_vaapi -R 44100 "$@"'';
  home = "vids";
}
