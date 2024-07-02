{
  config,
  lib,
  pkgs,
  ...
}: {
  environment = {
    sessionVariables.FLAKE = "/home/skykanin/dotfiles/nix";
    pathsToLink = ["/share/nix-direnv"];
  };

  documentation = {
    dev.enable = true;
    nixos.enable = true;
    man.generateCaches = true;
  };

  fontDir.enable = config.services.flatpak.enable;

  services = {
    gnome.gnome-keyring.enable = true;
    ratbagd.enable = true;
  };

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
      extraPackages = with pkgs; [openssh];
      package = pkgs.docker_24;
    };
    libvirtd.enable = true;
  };
}
