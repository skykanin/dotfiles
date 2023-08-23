{ config, idris2-main, pkgs, ... }:

let
  obs-studio-custom = pkgs.wrapOBS {
    plugins = with pkgs.obs-studio-plugins; [ input-overlay ];
  };
  vim-with-conf = (import ./../modules/vim.nix { inherit pkgs; });
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
    dotty
    emacs29
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
    # Fix discocss. See: https://github.com/mlvzk/discocss/issues/26
    (final: prev: {
      discocss = prev.discocss.overrideAttrs (oldAttrs: rec {
        src = oldAttrs.src // {
          rev = "dfb5a0c9c124c75081351df089e835aa7aca6d60";
          sha256 = "sha256-B4gGDav2Bere9FXl7RHQ+MeUxCHmtkXi4vdSC1O1JI8=";
        };
        # Make discocss work with Discord 0.0.28
        patches = [ ./discocss-fix.patch ];
      });
    })
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
  ];
}
