{ config, lib, pkgs, ... }:
{
  services.emacs = {
    enable = true;
    package =
      (pkgs.emacsPackagesFor (pkgs.emacs29.override {withPgtk = true;}))
        .emacsWithPackages (epkgs: [epkgs.vterm]);
  };
}
