let
  pkgs = import ./nixpkgs.nix;
in
  pkgs.writeShellApplication {
    name = "vidmaker";
    text = ''
      # Check if at least the output file argument is provided
      if [ "$#" -lt 1 ]; then
          echo "Usage: $0 output_file.mp4 [video1.mp4 video2.mp4 ...]"
          exit 1
      fi

      # The first argument is the output file
      OUTPUT_FILE=$1
      shift  # Shift to get the video files

      # If no video files are specified, use all files in the current directory
      if [ "$#" -eq 0 ]; then
          FILES=(*)
      else
          FILES=("$@")
      fi

      # Check if there are any files found
      if [ ''${#FILES[@]} -eq 0 ]; then
          echo "No video files found to concatenate."
          exit 1
      fi

      # Generate the file list dynamically
      TEMP_FILE="./vidmaker.cfg"
      for file in "''${FILES[@]}"; do
          echo "file '$file'" >> "$TEMP_FILE"
      done

      # Run ffmpeg with the generated file list
      ${pkgs.ffmpeg}/bin/ffmpeg -f concat -safe 0 -i "$TEMP_FILE" -c copy "$OUTPUT_FILE"

      # Clean up the temporary file
      rm "$TEMP_FILE"
    '';
  }
