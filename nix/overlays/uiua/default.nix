final: prev: {
  uiua = prev.uiua.overrideAttrs (finalAttrs: prevAttrs: {
    name = "uiua-${finalAttrs.version}";
    version = "0.7.1";

    src = prev.fetchFromGitHub {
      owner = "uiua-lang";
      repo = "uiua";
      rev = finalAttrs.version;
      hash = "sha256-cBwQdArVRiXH8TmgBSPpcB5oNu3Q/+Us9Azzw0lV5Vs=";
    };

    cargoDeps = prevAttrs.cargoDeps.overrideAttrs (_: {
      inherit (finalAttrs) src;
      outputHash = "sha256-7cgKiEqklvUw64a6+lbHA9t6QWiTquYVi0evXkONEag=";
    });
  });
}
