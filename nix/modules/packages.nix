{ config, pkgs, ... }:

with pkgs;
let stable = import <stable> { config.allowUnfree = true; };
in {
  environment.systemPackages = with pkgs; [
    adwaita-qt
    android-studio
    android-udev-rules
    arc-icon-theme
    arc-theme
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
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
    ffmpeg-full
    file
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
    jetbrains.idea-ultimate
    joker
    kitty
    lagrange
    my-leiningen
    lutris
    lxappearance
    mailspring
    metals
    mpv-with-scripts
    neofetch
    nix-direnv
    nixfmt
    nodejs-14_x
    obs-studio
    openjdk16
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
    shellcheck
    slack
    spotifywm
    steam
    texlive.combined.scheme-full
    tldr
    tree
    unzip
    vim_configurable
    weechat-custom
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
      weechat-custom = super.weechat.override {
        configure = { availablePlugins, ... }: {
          scripts = with super.weechatScripts; [ weechat-matrix ];
        };
      };
    })
    (self: super: {
      my-leiningen = super.leiningen.override { jdk = pkgs.openjdk11; };
    })
    (self: super: {
      idris2-git = idris2.overrideAttrs (oldAttrs: {
        name = "idris2";
        version = null;

        src = pkgs.fetchFromGitHub {
          owner = "idris-lang";
          repo = "Idris2";
          rev = "82cf4092b725cfac19b341ee6d1d8ff80bd84d61";
          sha256 = "171nm3vi9h5idlzr8s42vqyw2c8l034icg6hzj7bh0qbkxrpqxig";
        };
        buildFlags = [ "bootstrap" "SCHEME=scheme" ];
      });
    })
    (import (builtins.fetchTarball {
      url =
        "https://github.com/nix-community/emacs-overlay/archive/master.tar.gz";
      sha256 = "17iyyibsy26lvv39dim2x6hv4br726yrq29k43z9qyyr6dbp55sa";
    }))
  ];
}
