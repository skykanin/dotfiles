final: prev: {
  # Adds packages to PATH that are needed by the preview-tui NNN plugin
  # to work with additional file formats
  nnn = prev.nnn.overrideAttrs(oldAttrs: {
    nativeBuildInputs = (oldAttrs.nativeBuildInputs or [ ])
        ++ [ final.makeWrapper ];
    postInstall = ''
      ${oldAttrs.postInstall or ""}

      wrapProgram $out/bin/nnn \
        --prefix PATH : "${final.lib.makeBinPath (with final; [
          bat
          ffmpegthumbnailer
          poppler_utils
        ] ++ final.lib.optional final.stdenv.isLinux final.gnome-epub-thumbnailer)}"
    '';
  });
}
