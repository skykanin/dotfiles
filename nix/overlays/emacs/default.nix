final: prev: {
  # Add vterm package and build in pure GTK mode for wayland support
  emacs = (prev.emacsPackagesFor (prev.emacs29.override { withPgtk = true; }))
    .emacsWithPackages (epkgs: [epkgs.vterm]);
}
