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
in {
  environment.systemPackages = with pkgs; [
    adwaita-qt
    #android-studio
    #android-udev-rules
    ani-cli
    arc-icon-theme
    arc-theme
    autorandr
    bat
    betterdiscordctl
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
    emacsNativeComp
    fd
    feh
    ffmpeg-full
    file
    firefox-bin
    fish
    flameshot
    gitAndTools.gh
    haskell.compiler.ghc922
    htop
    gimp
    gitFull
    gnome3.nautilus
    networkmanagerapplet
    gnumake
    insomnia
    jetbrains.idea-ultimate
    joker
    kitty
    lagrange
    lutris
    lxappearance
    maim
    metals
    mpv-with-scripts
    neofetch
    nix-direnv
    nixfmt
    nodejs-17_x
    noisetorch
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
    streamlink
    tldr
    tree
    unzip
    vim-with-conf
    #vscode
    weechat-custom
    wget
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
    (final: prev:
      let
        patchedPkgs = import (builtins.fetchTarball {
          url =
            "https://github.com/nixos/nixpkgs/archive/ffdadd3ef9167657657d60daf3fe0f1b3176402d.tar.gz";
          sha256 = "1nrz4vzjsf3n8wlnxskgcgcvpwaymrlff690f5njm4nl0rv22hkh";
        }) {
          inherit (prev) system config;
          # inherit (prev) overlays;  # not sure
        };
        patchedPam = patchedPkgs.pam;
      in { i3lock-color = prev.i3lock-color.override { pam = patchedPam; }; })
    # xmonad.overlay
    # xmonad-contrib.overlay
  ];
}
