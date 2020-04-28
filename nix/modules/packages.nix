{ config, pkgs, ... }:

with pkgs;
let stable = import <stable> { config.allowUnfree = true; };
  in
{
  environment.systemPackages = with pkgs; [
    adwaita-qt
    android-studio
    arc-icon-theme
    stable.arc-theme
    ant-theme
    bat
    betterlockscreen
    cabal-install
    clojure
    curl
    colorz
    direnv
    discord
    docker
    (import /home/skykanin/dotfiles/nix/emacs.nix { inherit pkgs; }) # Emacs with my pkgs
    feh
    firefox
    fish
    flameshot
    (stable.haskellPackages.ghcWithPackages (ps: with ps; [hlint hindent]))
    gnome3.nautilus
    gnome3.networkmanagerapplet
    git
    jetbrains.idea-ultimate
    joker
    kitty
    my-leiningen
    lxappearance-gtk3
    mpv
    neofetch
    nodejs-13_x
    obs-studio
    pamixer
    pavucontrol
    polybarFull
    playerctl
    plex
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
      my-idea-ultimate = super.jetbrains.idea-ultimate.override {
        jdk = pkgs.jetbrains.jdk;
      };
    })
  ];
}
