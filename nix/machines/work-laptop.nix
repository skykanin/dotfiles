{
  config,
  options,
  pkgs,
  ...
}: {
  imports = [
    ../modules/bluetooth.nix
    ../modules/boot-work.nix
    ../modules/nix.nix
    ../modules/hardware.nix
    ../modules/general/default.nix
    ../modules/general/nixos.nix
    ../modules/networking.nix
    ../modules/programs/default.nix
    ../modules/programs/nixos.nix
    ../modules/overlays.nix
    ../modules/services/noisetorch.nix
    ../modules/services/polybar.nix
    ../modules/redshift.nix
    ../modules/sound.nix
    ../modules/ssh.nix
    ../modules/user.nix
    ../modules/desktop/xserver/default.nix
  ];

  # Local modules
  local = {
    networking = {
      firewall.enable = true;
      networkmanager.enable = true;
    };

    nix = {
      max-jobs = 6;
      extra-substituters = ["https://scrive.cachix.org"];
      extra-trusted-public-keys = ["scrive.cachix.org-1:U0qIgICaW+EuvCoqaYbbHR8JKTGNi29w4d+7Bc4LWfU="];
    };

    programs.light.enable = true;

    services = {
      polybar = {
        enable = true;
        startup-script = ''
          MONITOR=eDP-1-1 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
          MONITOR=DP-1-1 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
          MONITOR=DP-1-2 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
          MONITOR=DP-1-3 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
        '';
      };
      noisetorch = {
        enable = true;
        device-unit = "sys-devices-pci0000:00-0000:00:14.0-usb1-1\\x2d4-1\\x2d4.2-1\\x2d4.2:1.0-sound-card1-controlC1.device";
        device-id = "alsa_input.usb-0c76_USB_PnP_Audio_Device-00.mono-fallback";
      };

      xserver.xautolock = {
        enable = true;
        time = 10;
      };
    };
  };

  services.xserver.videoDrivers = ["nvidia"];

  networking.hostName = "iris";

  environment.systemPackages = with pkgs; [
    _1password-gui
    adwaita-qt
    arc-icon-theme
    arc-theme
    (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
    autorandr
    babashka
    bat
    blueberry
    bottom
    brightnessctl
    haskell.packages.ghc946.cabal-install
    cachix
    colorz
    curl
    difftastic
    direnv
    discord
    emacs
    fd
    feh
    ffmpeg-full
    file
    firefox-bin
    flameshot
    gimp
    git
    gitAndTools.gh
    gnumake
    haskell.packages.ghc946.ghc
    httpie-desktop
    htop
    insomnia
    jq
    keepassxc
    kitty
    lagrange
    libxml2
    lshw
    lxappearance
    maim
    mktemp
    mpv
    neofetch
    networkmanagerapplet
    nh
    nix-direnv
    nnn
    nodejs-18_x
    ntp
    openjdk
    openresolv
    openssl
    openvpn
    pamixer
    pavucontrol
    pciutils
    playerctl
    polybarFull
    python312
    pywal
    qdirstat
    ripgrep
    rlwrap
    rofi
    shellcheck
    slack
    spotifywm
    tldr
    tree
    unzip
    vim-with-conf
    xclip
    xsel
    zoom-us
  ];

  hardware.nvidia = {
    modesetting.enable = true;
    nvidiaSettings = true;
    nvidiaPersistenced = true;
    prime = {
      sync.enable = true;
      nvidiaBusId = "PCI:1:0:0";
      intelBusId = "PCI:0:2:0";
    };
  };

  programs._1password.enable = true;

  services = {
    logind.lidSwitch = "suspend";

    kolide-launcher = {
      enable = true;
      enrollSecretPath = "/persist/kolide/secret";
      rootDirectory = "/cache/kolide";
      additionalPackages = with pkgs; [glib zfs networkmanager cryptsetup];
    };

    ntp.enable = true;
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlan0.useDHCP = true;

  environment.extraInit = ''
    xrandr --output eDP-1-1 --mode 2560x1600
  '';

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}
