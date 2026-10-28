{ pkgs, inputs, ... }:

{
  imports = [
    ../modules/nix.nix
  ];

  # Use the systemd-boot EFI boot loader.
  boot.loader.systemd-boot.enable = true;
  boot.loader.efi.canTouchEfiVariables = true;

  # Use latest kernel.
  boot.kernelPackages = pkgs.linuxPackages_latest;

  networking.hostName = "hush"; # Define your hostname.

  # Enable networking
  networking.networkmanager.enable = true;

  # Set your time zone.
  time.timeZone = "Europe/Oslo";

  # Select internationalisation properties.
  i18n = {
    defaultLocale = "en_GB.UTF-8";
    extraLocaleSettings = {
      LC_ADDRESS = "nb_NO.UTF-8";
      LC_IDENTIFICATION = "nb_NO.UTF-8";
      LC_MEASUREMENT = "nb_NO.UTF-8";
      LC_MONETARY = "nb_NO.UTF-8";
      LC_NAME = "nb_NO.UTF-8";
      LC_NUMERIC = "nb_NO.UTF-8";
      LC_PAPER = "nb_NO.UTF-8";
      LC_TELEPHONE = "nb_NO.UTF-8";
      LC_TIME = "nb_NO.UTF-8";
    };
  };

  # Configure keymap in X11
  services.xserver.xkb = {
    layout = "us";
    options = "caps:escape,eurosign:e,compose:rctrl,compose:ralt";
  };

  programs = {
    fish.enable = true;
    nh = {
      enable = true;
      clean = {
        enable = true;
        dates = "monthly";
      };
    };
  };

  users.users."exec" = {
    isNormalUser = true;
    description = "exec";
    extraGroups = [ "networkmanager" "wheel" "docker" ];
    packages = with pkgs; [
      croc
      fastfetch
      ripgrep
      gitFull
      vim
    ];
    shell = pkgs.fish;
    openssh.authorizedKeys.keys = [
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIEemzAV7rW6hjqVIy1Pktd7jKtlkayVUNHfcO8H6SQfj hush server ssh key"
      "ssh-ed25519 AAAAC3NzaC1lZDI1NTE5AAAAIBIgYUNbgUiLAC6k+RgviJvi3DNyaGgENYmwE+Z/x+Zv meta@arch"
    ];
  };

  local.nix.settings.extra-trusted-users = [ "exec" ];

  # Enable the OpenSSH daemon.
  services.openssh = {
    enable = true;
    settings = {
      PasswordAuthentication = false;
      PermitRootLogin = "no";
    };
  };

  services.tangled.knot-rs = {
    enable = true;
    environmentFile = "/etc/knot/knot.env";
    settings = {
      server = {
        hostname = "knot.oyster.cafe";
        admins = ["did:plc:usucbdwz7frtkxjzxqtdtulu"];
        ssh_listen_addr = "[::]:22";
        appview_endpoint = "http://127.0.0.1:3000";
      };
      git.object_format = "sha256";
      atproto.plc_directory = "https://plc.directory";
      xrpc = {
        trusted_proxy_header = "x-forwarded-for";
        trusted_proxies = ["127.0.0.1" "::1"];
      };
    };
  };
  services.openssh.ports = [2200];

  services.tangled.appview = {
    enable = true;
    dev = true;
    appviewHost = "127.0.0.1:3000";
    appviewName = "Hush";
    listenAddr = "127.0.0.1:3000";
    metricsListenAddr = "127.0.0.1:9090";
    redis.addr = "127.0.0.1:6379";
    environmentFile = "/etc/tangled/appview.env";
  };

  # Development mode reads templates and static assets from the working directory.
  systemd.services.appview.preStart = ''
    mkdir -p appview/pages
    ln -sfn ${inputs.tangled}/appview/pages/templates appview/pages/templates
    ln -sfn ${inputs.tangled.packages.${pkgs.stdenv.hostPlatform.system}.appview-static-files} appview/pages/static
  '';

  services.caddy = {
    enable = true;
    virtualHosts = {
      "knot.oyster.cafe" = {
        extraConfig = ''
          reverse_proxy 127.0.0.1:5555
        '';
      };
    };
  };

  virtualisation.docker.enable = true;

  virtualisation.oci-containers = {
    backend = "docker";
    containers."core-keeper-dream" = {
      image = "escaping/core-keeper-dedicated";
      autoStart = true;

      environment = {
        WORLD_NAME = "Dream";
        MAX_PLAYERS = "10";
        WORLD_MODE = "1";
        WORLD_INDEX = "0";
      };

      volumes = [
        "/home/exec/core_keeper_dream/server-data:/home/steam/core-keeper-data"
      ];
    };
  };

  # Open ports in the firewall.
  networking.firewall.allowedTCPPorts = [ 22 53 80 443 2200 ];
  networking.firewall.allowedUDPPorts = [ 22 53 443 2200 ];

  security.pki.caCertificateBlacklist = [
    "Comodo AAA Services root"
    "emSign ECC Root CA - G3"
  ];

  # This option defines the first version of NixOS you have installed on this particular machine,
  # and is used to maintain compatibility with application data (e.g. databases) created on older NixOS versions.
  #
  # Most users should NEVER change this value after the initial install, for any reason,
  # even if you've upgraded your system to a new NixOS release.
  #
  # This value does NOT affect the Nixpkgs version your packages and OS are pulled from,
  # so changing it will NOT upgrade your system - see https://nixos.org/manual/nixos/stable/#sec-upgrading for how
  # to actually do that.
  #
  # This value being lower than the current NixOS release does NOT mean your system is
  # out of date, out of support, or vulnerable.
  #
  # Do NOT change this value unless you have manually inspected all the changes it would make to your configuration,
  # and migrated your data accordingly.
  #
  # For more information, see `man configuration.nix` or https://nixos.org/manual/nixos/stable/options#opt-system.stateVersion .
  system.stateVersion = "26.05"; # Did you read the comment?

}
