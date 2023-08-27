# Fix discocss. See: https://github.com/mlvzk/discocss/issues/26
final: prev: {
  discocss = prev.discocss.overrideAttrs (oldAttrs: rec {
    src =
      oldAttrs.src
      // {
        rev = "dfb5a0c9c124c75081351df089e835aa7aca6d60";
        sha256 = "sha256-B4gGDav2Bere9FXl7RHQ+MeUxCHmtkXi4vdSC1O1JI8=";
      };
    # Make discocss work with Discord 0.0.28
    patches = [./discocss-fix.patch];
  });
}
