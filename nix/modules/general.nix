{ config, pkgs, enableFirewall, enableNetworkmanager, enableOpengl ? true
, enableJellyfin ? false, polybarConfig, noisetorchConfig, threads
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

  hardware = {
    opengl = {
      driSupport = true;
      enable = enableOpengl;
      extraPackages = with pkgs; [
        vaapiVdpau
        libvdpau-va-gl
        intel-media-driver
      ];
     # Override broken nvidia config which depends on 32 bit `pkgsi686Linux.nvidia-vaapi-driver`
     # for `opengl.driSupport32Bit` which is enabled by the steam config.
     # Relevant link:
     # https://github.com/NixOS/nixpkgs/blob/6d6682772b62652b5019ffd7572cea1f39b72b20/nixos/modules/hardware/video/nvidia.nix#L395C45-L395C45
     extraPackages32 = pkgs.lib.mkForce [ pkgs.linuxPackages_latest.nvidia_x11.lib32 ];
    };
    opentabletdriver.enable = true;
  };

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

  nix = {
    settings = {
      auto-optimise-store = true;
      max-jobs = threads;
      substituters = [
        "https://cache.nixos.org"
        "https://iohk.cachix.org"
        "https://nix-community.cachix.org"
        "https://scrive.cachix.org"
      ];
      trusted-public-keys = [
        "cache.nixos.org-1:6NCHdD59X431o0gWypbMrAURkbJ16ZPMQFGspcDShjY="
        "iohk.cachix.org-1:DpRUyj7h7V830dp/i6Nti+NEO2/nhblbov/8MW7Rqoo="
        "nix-community.cachix.org-1:mB9FSh9qf2dCimDSUo8Zy7bkq5CX+/rkCWyvRCYg3Fs="
        "scrive.cachix.org-1:U0qIgICaW+EuvCoqaYbbHR8JKTGNi29w4d+7Bc4LWfU="
      ];
      trusted-substituters = [ "https://hydra.iohk.io" ];
      trusted-users = [ "root" "skykanin" ];
    };
    extraOptions = ''
      experimental-features = nix-command flakes repl-flake
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

  nixpkgs.config.allowUnfree = true;

  programs.noisetorch.enable = noisetorchConfig.enable;

  qt = {
    enable = true;
    platformTheme = "gtk2";
    style = "gtk2";
  };

  services = {
    custom = {
      polybar = polybarConfig;
      noisetorch = noisetorchConfig;
    };

    gnome.gnome-keyring.enable = true;
    jellyfin = {
      enable = enableJellyfin;
      openFirewall = true;
    };
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
