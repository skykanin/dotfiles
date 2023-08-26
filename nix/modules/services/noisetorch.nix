{ config, lib, pkgs, ... }:

let
  cfg = config.local.services.noisetorch;
in {
  options.local.services.noisetorch = with lib; {
    enable = mkEnableOption "Enable noisetorch service";
    package = mkOption {
      type = lib.types.package;
      default = pkgs.noisetorch;
      description = "The noisetorch package to be used for the daemon";
    };

    device-unit = mkOption {
      type = lib.types.str;
      description =
        "The systemd device unit of the microphone you want noisetorch to act on";
      example =
        "sys-devices-pci0000:00-0000:00:14.0-usb1-1\\x2d6-1\\x2d6:1.0-sound-card4-controlC4.device";
    };

    device-id = mkOption {
      type = lib.types.str;
      description =
        "The noisetorch device id of the microphone you want noisetorch to act on. Can be found with `noisetorch -l`";
      example =
        "alsa_input.usb-Blue_Microphones_Yeti_Stereo_Microphone_REV8-00.analog-stereo";
    };
  };

  config = lib.mkIf cfg.enable {

    programs.noisetorch.enable = true;

    systemd.user.services.noisetorch = {
      description = "Noisetorch Noise Cancelling Daemon";
      path = [ cfg.package ];
      serviceConfig = {
        Type = "simple";
        RemainAfterExit = "yes";
        ExecStart =
          "${cfg.package}/bin/noisetorch -i -s ${cfg.device-id} -t 95";
        ExecStop = "${cfg.package}/bin/noisetorch -u";
        Restart = "on-failure";
        RestartSec = 3;
      };
      requires = [ cfg.device-unit ];
      wantedBy = [ "default.target" ];
      after = [ "pulseaudio.service" cfg.device-unit ];
    };
  };
}
