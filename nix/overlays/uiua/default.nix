final: prev: {
  uiua = prev.uiua.overrideAttrs(finalAttrs: prevAttrs: {
    name = "uiua-${finalAttrs.version}";
    version = "0.6.1";

    src = prev.fetchFromGitHub {
      owner = "uiua-lang";
      repo = "uiua";
      rev = finalAttrs.version;
      hash = "sha256-/yeLsuwEKw6+jBKd7CAnR9RuVaKwXjVpcvO3v0FaAck=";
    };

    cargoDeps = prevAttrs.cargoDeps.overrideAttrs (_: {
      inherit (finalAttrs) src;
      outputHash = "sha256-4tR1n96s91EFZLO4RIBpNKLjOSbGrBIApJrS60RBuQQ=";
    });
  });
}
