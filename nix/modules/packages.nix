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
    arc-theme
    ant-theme
    autorandr
    bat
    betterlockscreen
    brightnessctl
    cachix
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
    mpv-with-scripts
    neofetch
    nix-direnv
    nodejs-14_x
    obs-studio-26
    pamixer
    pavucontrol
    polybarFull
    playerctl
    plex
    python3
    pywal
    qbittorrent
    qdirstat
    racket
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
      obs-studio-26 = obs-studio.overrideAttrs (oldAttrs: rec {
        pname = "obs-studio";
        version = "26.0.0-rc3";

        src = pkgs.fetchFromGitHub {
          owner = "obsproject";
          repo = "obs-studio";
          rev = version;
          sha256 = "0w14jfh6xi7rrz0p1ach3wmwmfkz7ff7nia66k6l9xc2brh3p8js";
        };

        buildInputs = oldAttrs.buildInputs ++ [pkgs.rnnoise];
      });
    })
    (self: super: {
      idris2-bleeding-edge = idris2.overrideAttrs (oldAttrs: {
        name = "idris2-master";
        version = null;
        
        src = pkgs.fetchFromGitHub {
          owner = "idris-lang";
          repo = "Idris2";
          rev = "master";
          sha256 = "1kimxyiajimgqlgnz0zy5qq41d0y3mb74vzkg2grymcqhz7zisyc";
        };
      });
    })
    (import (builtins.fetchTarball {
      url = https://github.com/nix-community/emacs-overlay/archive/master.tar.gz;
    }))
  ];
}
