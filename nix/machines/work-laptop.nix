{ config, emacs-overlay, pkgs, ... }:

let
  authorizedSshKeyFiles = [ "id_rsa" "id_rsa_github" ];
  enableFirewall = true;
  enableNetworkmanager = true;
  enableLight = true;
  polybar-script = ''
    MONITOR=eDP-1-1 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
    MONITOR=DP-1-1 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
    MONITOR=DP-1-3 polybar secondary -c /etc/polybar/config.ini &
  '';
  noisetorchConfig = {
    enable = true;
    device-unit = "";
    device-id = "";
  };
  threads = 6;
  xserverConfig = {
    compositorConfig = {
      enable = false;
      vSync = true;
    };
    videoDrivers = [ "nvidia" ];
    xautolockTimer = 5;
    xrandrHeads = [ ];
  };
in {
  imports = [
    ../modules/bluetooth.nix
    ../modules/boot-work.nix
    (import ../modules/general.nix {
      inherit config pkgs enableFirewall enableNetworkmanager noisetorchConfig
        polybar-script threads;
    })
    (import ../modules/programs.nix { inherit config pkgs enableLight; })
    ../modules/redshift.nix
    ../modules/sound.nix
    (import ../modules/ssh.nix { inherit config pkgs authorizedSshKeyFiles; })
    (import ../modules/user.nix { inherit config pkgs authorizedSshKeyFiles; })
    (import ../modules/xserver/xserver.nix
      ({ inherit config pkgs; } // xserverConfig))
  ];

  # Define hostname
  networking.hostName = "iris";

  environment.systemPackages = let
    vim-with-conf = pkgs.vim_configurable.customize {
      name = "vim";
      vimrcConfig.customRC = ''
        let mapleader = "<space>"
        map <leader>y "+y
        map <leader>p "+p

        syntax on
        set ruler
        set number
        set hlsearch
        set clipboard=unnamedplus
        set backspace=indent,eol,start
        set formatoptions=r
      '';
    };
  in with pkgs; [
    adwaita-qt
    android-studio
    arc-icon-theme
    arc-theme
    autorandr
    bat
    blueberry
    bottom
    brightnessctl
    cachix
    curl
    colorz
    direnv
    discord
    docker
    emacsNativeComp
    fd
    feh
    ffmpeg-full
    file
    firefox-bin
    fish
    gitAndTools.gh
    haskell.compiler.ghc922
    htop
    gimp
    git
    gnome3.nautilus
    networkmanagerapplet
    gnumake
    insomnia
    kitty
    lagrange
    lxappearance
    maim
    neofetch
    nix-direnv
    nodejs-17_x
    openjdk
    pamixer
    pavucontrol
    pciutils
    polybarFull
    playerctl
    python3
    pywal
    qdirstat
    ripgrep
    rlwrap
    rofi
    slack
    spotifywm
    tldr
    tree
    vim-with-conf
    xclip
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

  services.logind.lidSwitch = "suspend";

  nixpkgs.overlays = [
    (import emacs-overlay)
    (final: prev:
      let
        patchedPkgs = import (builtins.fetchTarball {
          url =
            "https://github.com/nixos/nixpkgs/archive/ffdadd3ef9167657657d60daf3fe0f1b3176402d.tar.gz";
          sha256 = "1nrz4vzjsf3n8wlnxskgcgcvpwaymrlff690f5njm4nl0rv22hkh";
        }) {
          inherit (prev) system config;
          # inherit (prev) overlays;  # not sure
        };
        patchedPam = patchedPkgs.pam;
      in { i3lock-color = prev.i3lock-color.override { pam = patchedPam; }; })
  ];

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;

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

