{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  stable-pkgs = import <stable> {};
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
  all-hies = import (fetchTarball "https://github.com/infinisil/all-hies/tarball/haskell.nix") {};
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    all-the-icons
    better-defaults
    cider
    clojure-mode
    company
    doom-modeline
    doom-themes
    flycheck
    flycheck-kotlin
    haskell-mode
    htmlize
    idris-mode
    impatient-mode
    jedi
    js-comint
    js2-mode
    lsp-mode
    markdown-mode
    markdown-preview-mode
    neotree
    nix-mode
    paredit
    parinfer
    projectile
    python-mode
    telephone-line
    use-package
    web-mode
    zenburn-theme
    zerodark-theme
  ]) ++ (with epkgs.melpaPackages; [
    company-coq
    elcord
    flycheck-clj-kondo
    flycheck-joker
    jedi-direx
    kotlin-mode
    lsp-haskell
    #org-link-minor-mode
    proof-general
  ]) ++ [
    (all-hies.selection { selector = p: { inherit (p) ghc883; }; })
    #pkgs.clj-kondo
    pkgs.gprolog
    pkgs.hlint
    pkgs.nodePackages.eslint
  ])

