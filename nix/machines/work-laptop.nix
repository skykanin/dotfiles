{ config, options, pkgs, ... }:

let
  enableFirewall = true;
  enableNetworkmanager = true;
  enableLight = true;
  enableSteam = false;
  polybarConfig = {
    enable = true;
    startup-script = ''
      MONITOR=eDP-1-1 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
      MONITOR=DP-1-1 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
      MONITOR=DP-1-2 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
      MONITOR=DP-1-3 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
    '';
  };
  noisetorchConfig = {
    enable = true;
    device-unit = "sys-devices-pci0000:00-0000:00:14.0-usb1-1\\x2d4-1\\x2d4.2-1\\x2d4.2:1.0-sound-card1-controlC1.device";
    device-id = "alsa_input.usb-0c76_USB_PnP_Audio_Device-00.mono-fallback";
  };
  threads = 6;
  xserverConfig = {
    compositorConfig = {
      enable = false;
      vSync = true;
    };
    videoDrivers = [ "nvidia" ];
    xautolockTimer = 20;
    xrandrHeads = [ ];
  };
in {
  imports = [
    ../modules/bluetooth.nix
    ../modules/boot-work.nix
    (import ../modules/general.nix {
      inherit config options pkgs enableFirewall enableNetworkmanager
        noisetorchConfig polybarConfig threads;
    })
    (import ../modules/programs.nix { inherit config pkgs enableLight enableSteam; })
    ../modules/redshift.nix
    ../modules/sound.nix
    (import ../modules/ssh.nix { inherit config pkgs; })
    (import ../modules/user.nix { inherit config pkgs; })
    (import ../modules/xserver/xserver.nix
      ({ inherit config pkgs; } // xserverConfig))
  ];

  networking.hostName = "iris";

  environment.systemPackages = let
    vim-with-conf = (import ./../modules/vim.nix { inherit pkgs; });
    in with pkgs; [
    _1password-gui-beta
    adwaita-qt
    arc-icon-theme
    arc-theme
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
    autorandr
    babashka
    bat
    blueberry
    bottom
    brightnessctl
    haskell.packages.ghc945.cabal-install
    cachix
    colorz
    curl
    difftastic
    direnv
    discord
    emacs29
    fd
    feh
    ffmpeg-full
    file
    firefox-bin
    fish
    flameshot
    gimp
    git
    gitAndTools.gh
    gnumake
    haskell.packages.ghc945.ghc
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
    python3
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
      additionalPackages = with pkgs; [ glib zfs networkmanager cryptsetup ];
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

