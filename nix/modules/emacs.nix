{pkgs, ...}: {
  services.emacs = {
    enable = true;
    package = let
      emacsPackage =
        if pkgs.stdenv.isLinux
        then pkgs.emacs30-pgtk
        else pkgs.emacs30;
    in
      emacsPackage.pkgs.emacsWithPackages (epkgs: [
        epkgs.vterm
        epkgs.treesit-grammars.with-all-grammars
      ]);
  };
}
