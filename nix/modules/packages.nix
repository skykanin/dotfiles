{ config, pkgs, ... }:

with pkgs;
let
  stable = import <stable> { config.allowUnfree = true; };
  in
{
  environment.systemPackages = with pkgs; [
    adwaita-qt
    android-studio
    arc-icon-theme
    stable.arc-theme
    ant-theme
    autorandr
    bat
    betterlockscreen
    chatterino2
    clojure
    curl
    colorz
    direnv
    discord
    docker
    dotty
    (import /home/skykanin/dotfiles/nix/emacs.nix { inherit pkgs; }) # Emacs with my pkgs
    fd
    feh
    firefox-devedition-bin
    fish
    flameshot
    gitAndTools.gh
    ghc
    gimp
    gnome3.nautilus
    gnome3.networkmanagerapplet
    git
    idris2-bleeding-edge
    jetbrains.idea-ultimate
    joker
    kitty
    my-leiningen
    lxappearance
    metals
    mpv
    neofetch
    nix-direnv
    nodejs-14_x
    obs-studio
    pamixer
    pavucontrol
    polybarFull
    playerctl
    plex
    python3
    pywal
    qbittorrent
    qdirstat
    rofi
    slack
    spotifywm
    steam
    tree
    unzip
    weechat
    wget
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
          sha256 = "17yhyig3g5s3ajqjjxp1k5mz71614acwjxczjbary46h8d136p1p";
        };
      });
    })
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];
}
