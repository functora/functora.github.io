{
  config,
  lib,
  pkgs,
  ...
}: let
  cfg = config.services.night-sleep;
in {
  options.services.night-sleep = {
    enable = lib.mkEnableOption "night-sleep";

    sleepAt = lib.mkOption {
      type = lib.types.str;
      default = "23:00:00";
    };

    wakeAt = lib.mkOption {
      type = lib.types.str;
      default = "tomorrow 08:00";
    };
  };

  config = lib.mkIf cfg.enable {
    environment.systemPackages = [pkgs.util-linux];

    systemd.services.night-sleep = {
      description = "Nightly sleep and wake";
      serviceConfig.Type = "oneshot";
      script = ''
        WAKE_AT="$(${pkgs.coreutils}/bin/date -d "${cfg.wakeAt}" +%s)"
        ${pkgs.util-linux}/bin/rtcwake -m off -t "$WAKE_AT"
      '';
    };

    systemd.timers.night-sleep = {
      description = "Timer for nightly sleep and wake";
      wantedBy = ["timers.target"];

      timerConfig = {
        OnCalendar = cfg.sleepAt;
        Unit = "night-sleep.service";
        Persistent = true;
        WakeSystem = true;
      };
    };
  };
}
