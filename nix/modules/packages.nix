{ config, emacs-overlay, idris2-main, pkgs, ... }:

with pkgs;
let
  vim-with-conf = (import ./../modules/vim.nix { inherit pkgs; });
  vscode = vscode-with-extensions.override {
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
    (aspellWithDicts (dicts: with dicts; [ en en-computers en-science ]))
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
    docker
    dotty
    emacs
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
    (haskell.packages.ghc944.ghcWithPackages (p: with p; [ unicode-show ]))
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
    obs-studio
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
    yt-dlp
    zip
    zoom-us
  ];

  # Nixpkgs overlays
  nixpkgs.overlays = [
    # This will require updating the two patch files in pywal nix package
    #
    # (self: super: {
    #   pywal-git = pywal.overrideAttrs (oldAttrs: {
    #     pname = "pywal-git";
    #     version = null;
    #     
    #     src = pkgs.fetchFromGitHub {
    #       owner = "dylanaraps";
    #       repo = "pywal";
    #       rev = "236aa48e741ff8d65c4c3826db2813bf2ee6f352";
    #       sha256 = "0p804c7f5f376vixd2ya5mxb2kdvwbmdb1ixs3j4cwf66sp89bid";
    #     };
    #     
    #     patches = [];
    #     postPatch = '''';
    #   }); 
    # })
    # Update CBQN to newer commit so that we get the latest features
    (final: prev: {
      cbqn = prev.cbqn.overrideAttrs (oldAttrs: {
        version = "0.pre+date=2023-02-01";
        src = pkgs.fetchFromGitHub {
          owner = "dzaima";
          repo = "CBQN";
          rev = "05c1270344908e98c9f2d06b3671c3646f8634c3";
          sha256 = "wKeyYWMgTZPr+Ienz3xnsXeD67vwdK4sXbQlW+GpQho=";
        };
      });
    })
    (import emacs-overlay)
  ];
}
