{ config, emacs-overlay, pkgs, ... }:

let
  authorizedSshKeyFiles = [ "id_rsa" "id_rsa_github" ];
  enableFirewall = true;
  enableLight = true;
  polybar-script = ''
    MONITOR=eDP-1-1 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
    MONITOR=DP-1-1 DEFAULT_BATTERY=BAT0 polybar primary -c /etc/polybar/config.ini &
    MONITOR=HDMI-0 polybar secondary -c /etc/polybar/config.ini &
  '';
  xserverConfig = {
    compositorConfig = {
      enable = false;
      vSync = true;
    };
    videoDrivers = [ "nvidia" ];
    xautolockTimer = 10;
    xrandrHeads = [ ];
  };
in {
  imports = [
    ../modules/bluetooth.nix
    ../modules/boot-work.nix
    (import ../modules/general.nix {
      inherit config pkgs enableFirewall polybar-script;
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

  environment.systemPackages = with pkgs; [
    adwaita-qt
    android-studio
    arc-icon-theme
    arc-theme
    autorandr
    bat
    bottom
    brightnessctl
    cachix
    curl
    colorz
    direnv
    discord
    docker
    emacsGcc
    fd
    feh
    ffmpeg-full
    file
    firefox-devedition-bin
    fish
    flameshot
    gitAndTools.gh
    haskell.compiler.ghc921
    gimp
    git
    gnome3.nautilus
    networkmanagerapplet
    gnumake
    insomnia
    kitty
    lagrange
    lxappearance
    neofetch
    nix-direnv
    nixfmt
    nodejs-14_x
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

  nixpkgs.overlays = [ (import emacs-overlay) ];

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

