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
    ghostty
    gucharmap
    htop
    httpie-desktop
    ifuse
    kdePackages.kdenlive
    lshw
    # Cross-platform, but currently broken on darwin.
    # Also currently insecure due to CVEs in libolm dependency.
    # See: https://github.com/Nheko-Reborn/nheko/issues/1786
    nheko
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
      btop
      cachix
      cbqn-replxx
      clojure
      croc
      curl
      difftastic
      discocss
      eza
      fastfetch
      fd
      ffmpeg-full
      file
      firefox
      gitAndTools.gh
      # gitFull broken on darwin atm
      (if pkgs.stdenv.isDarwin then git else gitFull)
      gnumake
      gnused
      idris2
      imagemagick
      iterm2
      jq
      jwt-cli
      keepassxc
      man-pages
      man-pages-posix
      mpv
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
      wget
      xdg-utils
      yq-go
      yt-dlp
      zip
    ])
    ++ lib.optionals pkgs.stdenv.isLinux linuxPackages;
}
