{ config, pkgs, ... }:

let unstable = import <nixos-unstable> { config.allowUnfree = true; }; in
{
  environment.systemPackages = with pkgs; [
    adwaita-qt
    unstable.android-studio
    arc-icon-theme
    arc-theme
    ant-theme
    bat
    betterlockscreen
    cabal-install
    clojure
    curl
    unstable.colorz
    unstable.discord
    docker
    (import /home/skykanin/dotfiles/nix/emacs.nix { inherit pkgs; }) # Emacs with my pkgs
    feh
    firefox
    fish
    flameshot
    ghc
    gnome3.nautilus
    gnome3.networkmanagerapplet
    git
    joker
    kitty
    my-idea-ultimate
    my-leiningen
    lxappearance-gtk3
    unstable.mpv-with-scripts
    neofetch
    pamixer
    pavucontrol
    polybarFull
    playerctl
    plex
    python38
    pywal
    qbittorrent
    rofi
    slack
    spotifywm
    weechat
    wget
    xdg_utils
    xorg.xbacklight
  ];

  # Nixpkgs overlays
  nixpkgs.overlays = [
    (self: super: {
      my-jbr = super.jetbrains.jdk.override {
        url = "https://jetbrains.bintray.com/intellij-jdk/jbr-11_0_2-linux-x64-b164.tar.gz";
      };
    })
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
