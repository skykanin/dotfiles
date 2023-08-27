{
  config,
  idris2-main,
  pkgs,
  ...
}: let
  obs-studio-custom = pkgs.wrapOBS {
    plugins = with pkgs.obs-studio-plugins; [input-overlay];
  };
  vim-with-conf = import ./../packages/vim.nix {inherit pkgs;};
  vscode = pkgs.vscode-with-extensions.override {
    vscodeExtensions = with pkgs.vscode-extensions; [
      ms-python.python
      ms-toolsai.jupyter
      ms-vsliveshare.vsliveshare
      vscodevim.vim
    ];
  };
in {
  environment.systemPackages = with pkgs; [
    # vscode
    #android-studio
    #android-udev-rules
    adwaita-qt
    ani-cli
    arc-icon-theme
    arc-theme
    (aspellWithDicts (dicts: with dicts; [en en-computers en-science]))
    autorandr
    babashka
    bat
    bintools-unwrapped
    blueberry
    bottom
    brightnessctl
    cachix
    cbqn-replxx
    chatterino2
    clj-kondo
    clojure
    colorz
    curl
    difftastic
    direnv
    discord
    discocss
    dotty
    ((emacsPackagesFor emacs29).emacsWithPackages (epkgs: [epkgs.vterm]))
    fd
    feh
    ffmpeg-full
    file
    firefox-bin
    fish
    flameshot
    gimp
    gitAndTools.gh
    gitFull
    gnumake
    (haskell.packages.ghc946.ghcWithPackages (p: with p; [unicode-show]))
    htop
    i3lock
    idris2-main.idris2
    insomnia
    imagemagick
    jetbrains.idea-community
    joker
    jq
    keepassxc
    kitty
    lagrange
    lshw
    lutris
    lxappearance
    man-pages
    man-pages-posix
    metals
    mpv
    neofetch
    networkmanagerapplet
    nix-direnv
    nnn
    nodejs-18_x
    noisetorch
    obs-studio-custom
    openjdk
    pamixer
    pavucontrol
    pciutils
    playerctl
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
    streamlink
    trash-cli
    tldr
    tree
    unison-ucm
    unzip
    vim-with-conf
    weechat
    wget
    xclip
    xdg-utils
    xorg.xbacklight
    xsel
    yt-dlp
    zip
    zoom-us
  ];
}
