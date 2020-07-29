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
    (import /home/skykanin/dotfiles/nix/emacs.nix { inherit pkgs; }) # Emacs with my pkgs
    feh
    firefox-devedition-bin
    fish
    flameshot
    ghc
    gnome3.nautilus
    gnome3.networkmanagerapplet
    git
    idris2-bleeding-edge
    jetbrains.idea-ultimate
    joker
    kitty
    my-leiningen
    lxappearance
    mpv
    neofetch
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
    weechat
    wget
    xdg_utils
    xorg.xbacklight
    youtube-dl
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
          sha256 = "0qni95vvl7p6sslf9s23i8z8cmw9nrzdrwkh434mjmdj9sg88gd9";
        };
      });
    })
  ];
}
