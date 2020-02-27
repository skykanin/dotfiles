{ pkgs ? import <nixpkgs> {} }:

let
  myEmacs = pkgs.emacs;
  emacsWithPackages = (pkgs.emacsPackagesNgGen myEmacs).emacsWithPackages;
in
  emacsWithPackages (epkgs: (with epkgs.melpaStablePackages; [
    better-defaults
    cider
    clojure-mode
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
    markdown-mode
    markdown-preview-mode
    neotree
    nix-mode
    paredit
    parinfer
    projectile
    python-mode
    smart-tabs-mode
    spaceline
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
    org-link-minor-mode
    proof-general
    spacemacs-theme
    xresources-theme
  ]))

