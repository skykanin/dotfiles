{ config, emacs-overlay, pkgs, xmonad, xmonad-contrib, ... }:

with pkgs;
let
  vim-with-conf = vim_configurable.customize {
    name = "vim";
    vimrcConfig.customRC = ''
      let mapleader = "<space>"
      map <leader>y "+y
      map <leader>p "+p

      syntax on
      set ruler
      set number
      set hlsearch
      set clipboard=unnamedplus
      set backspace=indent,eol,start
      set formatoptions=r
    '';
  };
  vscode = vscode-with-extensions.override {
    vscodeExtensions = with pkgs.vscode-extensions; [
      ms-python.python
      ms-toolsai.jupyter
      ms-vsliveshare.vsliveshare
      vscodevim.vim
    ];
  };
  # windowManagerPkgs = with pkgs; [
  #   i3lock-color
  #   rofi
  #   polybar-git
  # ];
in {
  environment.systemPackages = with pkgs; [
    adwaita-qt
    android-studio
    #android-udev-rules
    arc-icon-theme
    arc-theme
    aspell
    aspellDicts.en
    aspellDicts.en-computers
    aspellDicts.en-science
    autorandr
    bat
    blueberry
    bottom
    brightnessctl
    cachix
    chatterino2
    clojure
    clj-kondo
    curl
    colorz
    direnv
    discord
    docker
    dotty
    emacsGcc
    fd
    feh
    ffmpeg-full
    file
    firefox-devedition-bin
    fish
    gradle
    gitAndTools.gh
    haskell.compiler.ghc921
    htop
    gimp
    gitFull
    gnome3.nautilus
    gnome3.networkmanagerapplet
    gnumake
    insomnia
    jetbrains.idea-ultimate
    joker
    kitty
    lagrange
    my-leiningen
    lutris
    lxappearance
    mailspring
    maim
    metals
    mpv-with-scripts
    neofetch
    nix-direnv
    nixfmt
    nodejs-14_x
    obs-studio
    openjdk17
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
    rofiWithPlugins
    shellcheck
    slack
    spotifywm
    steam
    texlive.combined.scheme-full
    tldr
    tree
    unzip
    vim-with-conf
    #vscode
    weechat-custom
    wget
    winetricks
    wineWowPackages.full
    xclip
    xdg_utils
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
    (self: super: {
      rofiWithPlugins = let
        rofi-hoogle-src = pkgs.fetchFromGitHub {
          owner = "rebeccaskinner";
          repo = "rofi-hoogle";
          rev = "27c273ff67add68578052a13f560a08c12fa5767";
          sha256 = "09vx9bc8s53c575haalcqkdwy44ys1j8v9k2aaly7lndr19spp8f";
        };
        rofi-hoogle = import "${rofi-hoogle-src}/release.nix";
      in super.rofi.override { plugins = [ rofi-hoogle.rofi-hoogle ]; };
    })
    (self: super: {
      weechat-custom = super.weechat.override {
        configure = { availablePlugins, ... }: {
          scripts = with super.weechatScripts;
            [
              #weechat-matrix
            ];
        };
      };
    })
    (self: super: {
      my-leiningen = super.leiningen.override { jdk = pkgs.openjdk11; };
    })
    (self: super: {
      idris2-git = idris2.overrideAttrs (oldAttrs: {
        name = "idris2";
        version = null;

        src = pkgs.fetchFromGitHub {
          owner = "idris-lang";
          repo = "Idris2";
          rev = "82cf4092b725cfac19b341ee6d1d8ff80bd84d61";
          sha256 = "171nm3vi9h5idlzr8s42vqyw2c8l034icg6hzj7bh0qbkxrpqxig";
        };
        buildFlags = [ "bootstrap" "SCHEME=scheme" ];
      });
    })
    (self: super: {
      polybar-git = polybarFull.overrideAttrs (oldAttrs: rec {
        name = "polybar";
        version = "542f70efa3efd23e0305b0f728ae0389fdea4962";

        src = pkgs.fetchFromGitHub {
          owner = name;
          repo = name;
          rev = version;
          sha256 = "sha256-GnF1IlwHe1nnqtYJNGcWYDA6EuZJsMVqqFqeEeJrkJM=";
          fetchSubmodules = true;
        };

        buildInputs = oldAttrs.buildInputs ++ [ libuv ];

        nativeBuildInputs = oldAttrs.nativeBuildInputs
          ++ [ python3Packages.sphinx ];

        patches = [ ./polybar.patch ];
      });
    })
    (import emacs-overlay)
    # xmonad.overlay
    # xmonad-contrib.overlay
  ];
}
