{ config, pkgs, ... }:

with pkgs;
let
  stable = import <stable> { config.allowUnfree = true; };
  in
{
  environment.systemPackages = with pkgs; [
    adwaita-qt
    androidenv.androidPkgs_9_0.platform-tools
    android-studio
    arc-icon-theme
    arc-theme
    ant-theme
    autorandr
    bat
    betterlockscreen
    brightnessctl
    cachix
    chatterino2
    clojure
    clj-kondo
    curl
    colorz
    direnv
    discord
    docker
    dotty
    emacsGcc
    fd
    feh
    firefox-devedition-bin
    fish
    flameshot
    gradle
    gitAndTools.gh
    ghc
    gimp
    git
    gnome3.nautilus
    gnome3.networkmanagerapplet
    idris2
    jetbrains.idea-ultimate
    joker
    kitty
    my-leiningen
    lutris
    lxappearance
    mailspring
    metals
    mpv-with-scripts
    neofetch
    nix-direnv
    nodejs-14_x
    obs-studio
    openjdk15
    pamixer
    pavucontrol
    pciutils
    polybarFull
    playerctl
    plex
    python3
    pywal
    qbittorrent
    qdirstat
    racket
    ripgrep
    rlwrap
    rofi
    slack
    spotifywm
    steam
    texlive.combined.scheme-full
    tldr
    tree
    unzip
    vim_configurable
    weechat
    wget
    winetricks
    wineWowPackages.full
    xclip
    xdg_utils
    xorg.xbacklight
    youtube-dl
    zip
  ];

  # Nixpkgs overlays
  nixpkgs.overlays = [
    (self: super: {
      my-leiningen = super.leiningen.override {
        jdk = pkgs.openjdk11;
      };
    })
    (self: super: {
      idris2-bleeding-edge = idris2.overrideAttrs (oldAttrs: {
        name = "idris2-master";
        version = null;
        
        src = pkgs.fetchFromGitHub {
          owner = "idris-lang";
          repo = "Idris2";
          rev = "master";
          sha256 = "127zihakb035n1a0ky78362s1f5qbg4b92kmzz95frchc0kwg596";
        };
      });
    })
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];
}
