{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  stable-pkgs = import <stable> {};
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    all-the-icons
    better-defaults
    cider
    clojure-mode
    company
    direnv
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
    tide
    use-package
    web-mode
    zenburn-theme
    zerodark-theme
  ]) ++ (with epkgs.melpaPackages; [
    company-coq
    elcord
    fira-code-mode
    flycheck-clj-kondo
    flycheck-joker
    jedi-direx
    kotlin-mode
    lsp-haskell
    #org-link-minor-mode
    prettier-js
    proof-general
  ]) ++ [
    pkgs.haskellPackages.haskell-language-server
    pkgs.hunspell
    pkgs.hunspellDicts.en_GB-ise
    pkgs.hunspellDicts.en_US
    #pkgs.clj-kondo
    pkgs.gprolog
    pkgs.nodePackages.eslint
    pkgs.nodePackages.prettier
    pkgs.fira-code-symbols
  ])

