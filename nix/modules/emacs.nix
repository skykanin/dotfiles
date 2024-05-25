{
  pkgs,
  ...
}: {
  services.emacs = {
    enable = true;
    package =
      pkgs.emacs29-pgtk.pkgs.emacsWithPackages (epkgs: [epkgs.vterm]);
  };
}
