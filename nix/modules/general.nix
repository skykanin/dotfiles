{
  config,
  pkgs,
  ...
}: {
  environment = {
    etc."cachix.nix".source = toString ./cachix.nix;

    pathsToLink = ["/share/nix-direnv"];

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

  fonts.packages = with pkgs; [bqn386 jetbrains-mono noto-fonts unifont victor-mono];

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
        features = {buildkit = true;};
      };
      package = pkgs.docker_24;
    };
    libvirtd.enable = true;
  };
}
