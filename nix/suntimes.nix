{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.functora.suntimes;
  suntimes-notify-script = import ./bubble.nix {
    inherit pkgs;
    name = "suntimes-notify";
    runtimeInputs = [pkgs.coreutils pkgs.libnotify pkgs.mako pkgs.sunwait];
    mkExtend = _: {
      timeZone.enable = true;
      dbus = {
        enable = true;
        policies = {
          "org.freedesktop.Notifications" = "talk";
        };
      };
    };
    text = ''
      # Show a persistent, critical notification whenever there is no sun
      # (from sunset to sunrise), helping to reduce screen time in the dark.

      lat="${cfg.latitude}"
      lon="${cfg.longitude}"

      case "$lat" in
        -*) latDir="S";;
        *)  latDir="N";;
      esac
      case "$lon" in
        -*) lonDir="W";;
        *)  lonDir="E";;
      esac
      latVal=$(printf '%s' "$lat" | sed 's/^-//')
      lonVal=$(printf '%s' "$lon" | sed 's/^-//')

      dayNight="$(${pkgs.sunwait}/bin/sunwait poll "$latVal"$latDir "$lonVal"$lonDir)" || true

      case "$dayNight" in
        NIGHT)
          # Clear the previous notification so only one exists at a time.
          ${pkgs.mako}/bin/makoctl dismiss --all --no-history || true

          times="$(${pkgs.sunwait}/bin/sunwait list "$latVal"$latDir "$lonVal"$lonDir)" || true
          sunrise=$(printf '%s' "$times" | cut -d, -f1)
          sunset=$(printf '%s' "$times" | cut -d, -f2 | sed 's/^ *//;s/ *$//')

          now="$(date '+%Y-%m-%d %A %H:%M')"

          body=$(printf '%s | Sunset: %s | Sunrise: %s' \
            "$now" "$sunset" "$sunrise")

          ${pkgs.libnotify}/bin/notify-send \
            -u critical \
            -t 0 \
            -a "suntimes" \
            "Suntimes - night time" \
            "$body" \
            || true
          ;;
        *)
          ;;
      esac
    '';
  };
in {
  options.services.functora.suntimes = {
    enable = lib.mkEnableOption "suntimes";

    latitude = lib.mkOption {
      type = lib.types.str;
      default = "59.4370";
      description = ''
        Latitude of the location, as a signed decimal (positive = north).
      '';
    };

    longitude = lib.mkOption {
      type = lib.types.str;
      default = "24.7536";
      description = ''
        Longitude of the location, as a signed decimal (positive = east).
      '';
    };
  };

  config = lib.mkMerge [
    (lib.mkIf cfg.enable {
      systemd.user.services.suntimes-notify = {
        description = "Suntimes night-time notification";
        serviceConfig = {
          Type = "oneshot";
        };
        script = "${suntimes-notify-script}/bin/suntimes-notify";
      };

      systemd.user.timers.suntimes-notify = {
        description = "Timer for suntimes night-time notification";
        wantedBy = ["timers.target"];

        timerConfig = {
          OnCalendar = "*-*-* *:*:0";
          Unit = "suntimes-notify.service";
          Persistent = true;
        };
      };
    })
  ];
}
