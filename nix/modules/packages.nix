{ config, pkgs, ... }:

with pkgs;
let stable = import <stable> { config.allowUnfree = true; };
  in
{
  environment.systemPackages = with pkgs; [
    adwaita-qt
    android-studio
    arc-icon-theme
    arc-theme
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
    # ghc
    gnome3.nautilus
    gnome3.networkmanagerapplet
    git
    joker
    kitty
    my-idea-ultimate
    my-leiningen
    lxappearance-gtk3
    mpv
    neofetch
    obs-studio
    pamixer
    pavucontrol
    polybarFull
    playerctl
    plex
    pywal
    qbittorrent
    rofi
    slack
    spotifywm
    steam
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
