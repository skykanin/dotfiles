{ config, pkgs, enableFirewall, enablePlex ? false, ... }:

{
  environment = {
    etc."cachix.nix".source = toString ./cachix.nix;

    pathsToLink = [ "/share/nix-direnv" ];

    variables.EDITOR = "vim";
  };

  fonts = {
    fonts = with pkgs; [
      jetbrains-mono
      noto-fonts
      unifont
      victor-mono
    ];
  };

  hardware = {
    opengl = {
      driSupport = true;
      driSupport32Bit = true;
      enable = true;
      extraPackages = with pkgs; [
        vaapiVdpau
        libvdpau-va-gl
        intel-media-driver
      ];
    };
  };

  networking = {
    firewall.enable = enableFirewall;
    firewall.allowedTCPPorts =
      [ 80 8080 3333 4568 9630 44330 19000 19001 19002 19003 ];
    firewall.allowedUDPPorts = [ 80 9630 44330 19000 19001 19002 19003 ];
    nameservers = [ "9.9.9.11" ];
    networkmanager.enable = true;
  };

  nix = {
    autoOptimiseStore = true;
    binaryCaches = [
      "https://hydra.iohk.io"
      "https://iohk.cachix.org"
      "https://nix-community.cachix.org"
      "https://scrive.cachix.org"
    ];
    binaryCachePublicKeys = [
      "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
      "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
      "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
      "scrive.cachix.org-1:U0qIgICaW+EuvCoqaYbbHR8JKTGNi29w4d+7Bc4LWfU="
    ];
    extraOptions = ''
      experimental-features = nix-command flakes
      keep-outputs = false
      keep-derivations = false
    '';
    gc = {
      automatic = true;
      persistent = true;
      dates = "weekly";
      options = "--delete-older-than 14d";
    };
    maxJobs = 2;
    package = pkgs.nixFlakes;
    trustedBinaryCaches = [ "https://hydra.iohk.io" ];
    trustedUsers = [ "root" "skykanin" ];
  };

  nixpkgs.config.allowUnfree = true;

  qt5 = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };

  services = {
    custom.polybar.enable = true;
    gnome.gnome-keyring.enable = true;
    plex = {
      enable = enablePlex;
      openFirewall = true;
    };
    ratbagd.enable = true;
  };

  system.autoUpgrade.enable = true;
  system.autoUpgrade.allowReboot = true;

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

  virtualisation = { docker.enable = true; };
}
