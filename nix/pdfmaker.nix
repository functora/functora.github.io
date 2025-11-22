let
  pkgs = import ./nixpkgs.nix;
in
  pkgs.writeShellApplication {
    name = "pdfmaker";
    text = ''
      set -euo pipefail

      INPUT="$1"
      BASENAME="$(basename "$INPUT" .md)"
      OUTPUT="$BASENAME.pdf"

      TMP="$(mktemp -d)"
      WORK="$TMP/processed.md"
      cp "$INPUT" "$WORK"

      LINKS=$(sed -n 's/.*(\(http[^)]*\)).*/\1/p' "$INPUT" | sort -u)

      i=1
      for URL in $LINKS; do
          PNG="$TMP/qr_$i.png"
          PDF="$TMP/qr_$i.pdf"

          qrencode -o "$PNG" -s 3 -m 1 "$URL"

          magick "$PNG" "$PDF"

          ESCAPED=$(printf '%s\n' "$URL" | sed -e 's/[\/&]/\\&/g')
          sed -i "s|($ESCAPED)|($ESCAPED)\n\n![]($PDF){ width=2cm }|g" "$WORK"

          i=$((i+1))
      done

      pandoc "$WORK" --standalone --pdf-engine=xelatex -o "$OUTPUT"
    '';

    runtimeInputs = with pkgs; [
      pandoc
      busybox
      qrencode
      texliveSmall
      imagemagick
    ];
  }
