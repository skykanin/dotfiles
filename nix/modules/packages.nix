{
  config,
  pkgs,
  ...
}: let
  obs-studio-custom = pkgs.wrapOBS {
    plugins = with pkgs.obs-studio-plugins; [input-overlay];
  };
  # TODO: If used, implement as an overlay
  vscode = pkgs.vscode-with-extensions.override {
    vscodeExtensions = with pkgs.vscode-extensions; [
      ms-python.python
      ms-toolsai.jupyter
      ms-vsliveshare.vsliveshare
      vscodevim.vim
    ];
  };
in {
  # Core packages for desktop systems
  environment.systemPackages = with pkgs; [
    (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
    babashka
    bat
    bintools-unwrapped
    blueberry
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
    feh
    ffmpeg-full
    file
    firefox-devedition
    fish
    foliate
    gitAndTools.gh
    gitFull
    gnome.gucharmap
    gnumake
    ghc
    htop
    idris2
    imagemagick
    jq
    keepassxc
    kitty
    lshw
    magic-wormhole
    man-pages
    man-pages-posix
    mpv
    neofetch
    nh
    nnn
    nodejs
    noisetorch
    openjdk
    pamixer
    pavucontrol
    pciutils
    playerctl
    python312
    pywal
    qdirstat
    ripgrep
    rlwrap
    rnote
    shellcheck
    slack
    spot
    spotifywm
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
  ];
}
