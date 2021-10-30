{ config, emacs-overlay, pkgs, ... }:

{
  imports =
    [ ../modules/bluetooth.nix
      ../modules/boot-work.nix
      ../modules/general.nix
      ../modules/programs.nix
      ../modules/redshift.nix
      ../modules/sound.nix
      ../modules/ssh.nix
      ../modules/user.nix
      ../modules/xserver.nix
    ];

  # Define hostname
  networking.hostName = "iris";

  programs.light.enable = true;

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
      gnome3.networkmanagerapplet
      gnumake
      kitty
      lagrange
      lxappearance
      neofetch
      nix-direnv
      nixfmt
      nodejs-14_x
      openjdk16
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
  ];

  nixpkgs.overlays = [
    (import emacs-overlay)
  ];

  services = {
    compton.vSync = true;
    logind.lidSwitch = "suspend";
    xserver.libinput.touchpad.naturalScrolling = false;
  };

  # The global useDHCP flag is deprecated, therefore explicitly set to false here.
  # Per-interface useDHCP will be mandatory in the future, so this generated config
  # replicates the default behaviour.
  networking.useDHCP = false;
  networking.interfaces.wlp0s20f3.useDHCP = true;
  networking.interfaces.enp0s20f0u5u1.useDHCP = true;

  # This value determines the NixOS release from which the default
  # settings for stateful data, like file locations and database versions
  # on your system were taken. It‘s perfectly fine and recommended to leave
  # this value at the release version of the first install of this system.
  # Before changing this value read the documentation for this option
  # (e.g. man configuration.nix or on https://nixos.org/nixos/options.html).
  system.stateVersion = "21.05"; # Did you read the comment?
}

