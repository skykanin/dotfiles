{pkgs, ...}: {
  services.emacs = {
    enable = true;
    package = let
      emacsPackage =
        if pkgs.stdenv.isLinux
        then pkgs.emacs-pgtk
        else pkgs.emacs-macport;
    in
      emacsPackage.pkgs.emacsWithPackages (epkgs: [
        pkgs.stdenv.cc
        epkgs.vterm
        epkgs.treesit-grammars.with-all-grammars
      ]);
  };
}
