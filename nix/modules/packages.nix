{
  config,
  lib,
  pkgs,
  ...
}: let
  # TODO: If used, implement as an overlay
  vscode = pkgs.vscode-with-extensions.override {
    vscodeExtensions = with pkgs.vscode-extensions; [
      ms-python.python
      ms-toolsai.jupyter
      ms-vsliveshare.vsliveshare
      vscodevim.vim
    ];
  };
  linuxPackages = with pkgs; [
    blueberry
    feh
    firefox-devedition
    foliate
    gnome.gucharmap
    htop
    lshw
    pamixer
    pavucontrol
    playerctl
    qdirstat
    rnote
    spotifywm
  ];
in {
  # Core unix packages
  environment.systemPackages = with pkgs;
    [
      (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
      babashka
      bat
      bintools-unwrapped
      bottom
      cachix
      cbqn-replxx
      clojure
      curl
      difftastic
      discord
      discocss
      emacs
      fd
      ffmpeg-full
      file
      gitAndTools.gh
      gitFull
      gnumake
      idris2
      imagemagick
      jq
      jwt-cli
      keepassxc
      kitty
      magic-wormhole
      man-pages
      man-pages-posix
      mpv
      neofetch
      nnn
      pciutils
      pywal
      ripgrep
      rlwrap
      shellcheck
      slack
      trash-cli
      tldr
      tree
      uiua
      unzip
      weechat
      wget
      xdg-utils
      yt-dlp
      zip
    ]
    ++ lib.optionals pkgs.stdenv.isLinux linuxPackages;
}
