final: prev: {
  uiua = prev.uiua.overrideAttrs(finalAttrs: prevAttrs: {
    name = "uiua-${finalAttrs.version}";
    version = "0.4.1";

    src = prev.fetchFromGitHub {
      owner = "uiua-lang";
      repo = "uiua";
      rev = finalAttrs.version;
      hash = "sha256-a5KSB2i7xg1b6aaNhdlU171wZ1REmFUtnsCWsKIHFGE=";
    };

    cargoDeps = prevAttrs.cargoDeps.overrideAttrs (_: {
      inherit (finalAttrs) src;
      outputHash = "sha256-iaczBzzGiw/+yFDnW9LA909HkICq+pksHdWK8lQvL6Y=";
    });
  });
}
