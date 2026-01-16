let
  pkgs = import ./nixpkgs.nix;
in
  pkgs.writeShellApplication {
    name = "trimout";

    runtimeInputs = [
      pkgs.ffmpeg
      pkgs.busybox
    ];

    text = ''
      set -euo pipefail

      if [ "$#" -ne 4 ]; then
        echo "Usage:"
        echo "  $0 <input_video> <trim_start_sec> <trim_end_sec> <fade_duration_sec>"
        exit 1
      fi

      INPUT="$1"
      TRIM_START="$2"
      TRIM_END="$3"
      FADE="$4"

      if [ ! -f "$INPUT" ]; then
        echo "Error: input file not found"
        exit 1
      fi

      for v in "$TRIM_START" "$TRIM_END" "$FADE"; do
        echo "$v" | grep -Eq '^[0-9]+(\.[0-9]+)?$' || {
          echo "Error: numeric arguments required (seconds)"
          exit 1
        }
      done

      DURATION=$(ffprobe -v error \
        -show_entries format=duration \
        -of default=noprint_wrappers=1:nokey=1 \
        "$INPUT")

      if [ -z "$DURATION" ]; then
        echo "Error: could not read video duration"
        exit 1
      fi

      NEW_DURATION=$(awk "BEGIN { print $DURATION - $TRIM_START - $TRIM_END }")

      if awk "BEGIN { exit !($NEW_DURATION <= 0) }"; then
        echo "Error: trimming removes entire video"
        exit 1
      fi

      FADE_START=$(awk "BEGIN { print $NEW_DURATION - $FADE }")

      if awk "BEGIN { exit !($FADE_START < 0) }"; then
        FADE_START=0
      fi

      BASE=$(basename "$INPUT")
      NAME="''${BASE%.*}"
      EXT="''${BASE##*.}"
      OUTPUT="''${NAME}_trimmed_fadeout.''${EXT}"

      echo "Input duration: $DURATION"
      echo "Trim start:     $TRIM_START"
      echo "Trim end:       $TRIM_END"
      echo "New duration:   $NEW_DURATION"
      echo "Fade start:     $FADE_START"
      echo "Fade duration:  $FADE"
      echo "Output:         $OUTPUT"
      echo

      FILTER="[0:v]fps=60:round=near,setpts=PTS-STARTPTS,fade=t=out:st=''${FADE_START}:d=''${FADE},format=yuv420p[v];[0:a]asetpts=PTS-STARTPTS,afade=t=out:st=''${FADE_START}:d=''${FADE},aresample=async=1:min_hard_comp=0.300:first_pts=0,aresample=48000:osr=48000[a]"

      ffmpeg -y \
        -fflags +genpts \
        -avoid_negative_ts make_zero \
        -i "$INPUT" \
        -ss "$TRIM_START" \
        -t "$NEW_DURATION" \
        -r 60 \
        -vsync cfr \
        -filter_complex "$FILTER" \
        -map "[v]" -map "[a]" \
        -movflags +faststart \
        -c:v libx264 \
        -preset medium \
        -crf 18 \
        -profile:v high \
        -level:v 4.2 \
        -bf 2 \
        -g 120 \
        -c:a aac \
        "$OUTPUT"

      echo "Done: $OUTPUT"
    '';
  }
