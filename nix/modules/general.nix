{ config, pkgs, enableFirewall, enableNetworkmanager, enableJellyfin ? false
, polybar-script, noisetorchConfig, threads, ... }:

{
  environment = {
    etc."cachix.nix".source = toString ./cachix.nix;

    pathsToLink = [ "/share/nix-direnv" ];

    variables.EDITOR = "vim";
  };

  fonts = {
    fonts = with pkgs; [ jetbrains-mono noto-fonts unifont victor-mono ];
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
    opentabletdriver.enable = true;
  };

  networking = {
    firewall.enable = enableFirewall;
    firewall.allowedTCPPorts =
      [ 80 8080 3333 4568 9630 44330 19000 19001 19002 19003 ];
    firewall.allowedUDPPorts = [ 80 9630 44330 19000 19001 19002 19003 ];
    #nameservers = [ "9.9.9.11" "8.8.8.8" ];
    networkmanager.enable = enableNetworkmanager;
  };

  nix = {
    settings = {
      auto-optimise-store = true;
      max-jobs = threads;
      substituters = [
        "https://cache.nixos.org"
        "https://hydra.iohk.io"
        "https://iohk.cachix.org"
        "https://nix-community.cachix.org"
        "https://scrive.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "hydra.iohk.io:f/Ea+s+dFdN+3Y/G+FDgSq+a5NEWhJGzdjvKNGv0/EQ="
        "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "scrive.cachix.org-1:U0qIgICaW+EuvCoqaYbbHR8JKTGNi29w4d+7Bc4LWfU="
      ];
      trusted-substituters = [ "https://hydra.iohk.io" ];
      trusted-users = [ "root" "skykanin" ];
    };
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
    package = pkgs.nixVersions.stable;
  };

  nixpkgs.config = {
    allowUnfree = true;
    # permittedInsecurePackages = [ "NoiseTorch-0.11.5" ];
  };

  programs.noisetorch.enable = true;

  qt5 = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };

  services = {
    custom = {
      polybar = {
        enable = true;
        startup-script = polybar-script;
      };
      noisetorch = {
        enable = noisetorchConfig.enable;
        device-unit = noisetorchConfig.device-unit;
        device-id = noisetorchConfig.device-id;
      };
    };

    gnome.gnome-keyring.enable = true;
    jellyfin = {
      enable = true;
      openFirewall = true;
    };
    ratbagd.enable = true;
  };

  system = {
    autoUpgrade = {
      allowReboot = true;
      enable = true;
    };
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
    docker.enable = true;
    libvirtd.enable = true;
  };
}
