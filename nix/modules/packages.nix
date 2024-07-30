{
  lib,
  pkgs,
  ...
}: let
  # TODO: If used, implement as an overlay
  _vscode = pkgs.vscode-with-extensions.override {
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
    foliate
    gucharmap
    htop
    ifuse
    kdenlive
    kitty
    # Although librewolf is cross-platform the nixpkgs package
    # is broken on darwin because firefox is broken for darwin
    librewolf
    lshw
    pamixer
    pavucontrol
    playerctl
    qdirstat
    rnote
    spotifywm
    usbutils
    vesktop
  ];
in {
  # Core unix packages
  environment.systemPackages =
    (with pkgs; [
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
      eza
      fastfetch
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
      man-pages
      man-pages-posix
      mpv
      nheko
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
      wormhole-william
      wget
      xdg-utils
      yt-dlp
      zip
    ])
    ++ lib.optionals pkgs.stdenv.isLinux linuxPackages;
}
