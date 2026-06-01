{
  config,
  lib,
  pkgs,
  ...
}: {
  environment = {
    sessionVariables.NH_FLAKE = "/home/skykanin/dotfiles/nix";
    pathsToLink = ["/share/nix-direnv"];
  };

  documentation = {
    dev.enable = true;
    nixos.enable = true;
    man.cache.enable = true;
  };

  services = {
    gnome.gnome-keyring.enable = true;
    ratbagd.enable = true;
    timesyncd.enable = true;
  };

  time = {
    hardwareClockInLocalTime = false;
    timeZone = "Europe/Oslo";
  };

  virtualisation = {
    docker = {
      enable = true;
      daemon.settings = {
        features = {buildkit = true;};
      };
      extraPackages = with pkgs; [openssh];
    };
    libvirtd.enable = true;
  };
}
