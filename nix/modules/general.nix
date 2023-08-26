{ config, pkgs, enableFirewall, enableNetworkmanager
, options, ... }:

{
  environment = {
    etc."cachix.nix".source = toString ./cachix.nix;

    pathsToLink = [ "/share/nix-direnv" ];

    variables.EDITOR = "vim";
  };

  documentation = {
    enable = true;
    dev.enable = true;
    doc.enable = true;
    info.enable = true;
    man = {
      enable = true;
      generateCaches = true;
    };
    nixos.enable = true;
  };

  fonts.packages= with pkgs; [ bqn386 jetbrains-mono noto-fonts unifont victor-mono ];

  networking = {
    firewall.enable = enableFirewall;
    firewall.allowedTCPPorts =
      [ 80 8080 3333 4568 9630 44330 19000 19001 19002 19003 ];
    firewall.allowedUDPPorts = [ 80 9630 44330 19000 19001 19002 19003 ];
    #nameservers = [ "9.9.9.11" "8.8.8.8" ];
    networkmanager = {
       enable = enableNetworkmanager;
       wifi.backend = "iwd";
    };
    timeServers = options.networking.timeServers.default ++ [ "ntp.example.com" ];
  };

  qt = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };

  services = {
    gnome.gnome-keyring.enable = true;
    ratbagd.enable = true;
  };

  systemd.extraConfig = ''
    DefaultTimeoutStartSec=1s
    DefaultTimeoutStopSec=1s
    DefaultTimeoutRestartSec=1s
    DefaultTimeoutAbortSec=1s
  '';

  time = {
    hardwareClockInLocalTime = true;
    timeZone = "Europe/Oslo";
  };

  virtualisation = {
    docker = {
      enable = true;
      daemon.settings = {
        features = { buildkit = true; };
      };
      package = pkgs.docker_24;
    };
    libvirtd.enable = true;
  };
}
