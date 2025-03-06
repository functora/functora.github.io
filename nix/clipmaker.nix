let
  pkgs = import ./nixpkgs.nix;
in
  pkgs.writeShellApplication {
    name = "clipmaker";
    text = ''
      # Check if there are at least two arguments (video and at least one audio file)
      if [ "$#" -lt 2 ]; then
        echo "Usage: $0 <video_file> <audio_file1> [audio_file2 ...]"
        exit 1
      fi

      # The first argument is the video file
      VIDEO_FILE="$1"
      shift  # Remove the first argument (video file) from the list

      # The rest are audio files
      AUDIO_FILES=("$@")

      # Get the video duration
      VID_LEN_RAW="$(${pkgs.ffmpeg}/bin/ffprobe -v error -select_streams v:0 -show_entries stream=duration -of default=noprint_wrappers=1:nokey=1 "$VIDEO_FILE")"
      VID_LEN_INT="$(LC_NUMERIC=C printf "%.0f\n" "$VID_LEN_RAW")"

      # Construct the filter_complex dynamically based on the number of audio files
      FILTER_COMPLEX=""

      # Create the concat filter part for audio
      for i in "''${!AUDIO_FILES[@]}"; do
        FILTER_COMPLEX+="[$((i+1)):a]"
      done

      # Concatenate the audio files and apply fade-out
      FILTER_COMPLEX+="concat=n=''${#AUDIO_FILES[@]}:v=0:a=1[a]; [a]atrim=end=''${VID_LEN_RAW},afade=t=out:st=$((VID_LEN_INT - 5)):d=5[vout]"

      # Construct the ffmpeg command, creating the input arguments in a safe manner
      INPUT_ARGS=("-i" "$VIDEO_FILE")
      for audio in "''${AUDIO_FILES[@]}"; do
        INPUT_ARGS+=("-i" "$audio")
      done

      ${pkgs.ffmpeg}/bin/ffmpeg "''${INPUT_ARGS[@]}" \
        -filter_complex "$FILTER_COMPLEX" \
        -map 0:v -map "[vout]" -c:v copy -y output.mp4
    '';
  }
