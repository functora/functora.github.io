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
      HEADER="$TMP/header.tex"

      echo '\usepackage{graphicx}' > "$HEADER"

      cp "$INPUT" "$WORK"

      LINKS=$(sed -n 's/.*(\(http[^)]*\)).*/\1/p' "$INPUT" | sort -u)

      i=1
      for URL in $LINKS; do
          PNG="$TMP/qr_$i.png"
          PDF="$TMP/qr_$i.pdf"

          qrencode -o "$PNG" -s 3 -m 1 "$URL"
          magick "$PNG" "$PDF"

          ESCAPED=$(printf '%s\n' "$URL" | sed -e 's/[\/&]/\\&/g')

          sed -i "s|($ESCAPED)|($ESCAPED)\n\n\\\\begin{center}\n\\\\includegraphics[width=2cm]{$PDF}\n\\\\end{center}\n|g" "$WORK"

          i=$((i+1))
      done

      pandoc "$WORK" --standalone --pdf-engine=xelatex --include-in-header="$HEADER" -o "$OUTPUT"
    '';

    runtimeInputs = with pkgs; [
      pandoc
      busybox
      qrencode
      texliveSmall
      imagemagick
    ];
  }
