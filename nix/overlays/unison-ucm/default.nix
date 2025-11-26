final: prev: {
  unison-ucm = prev.unison-ucm.overrideAttrs (finalAttrs: prevAttrs: {
    version = "1.0.0";
    src = {
      aarch64-darwin = final.fetchurl {
        url = "https://github.com/unisonweb/unison/releases/download/release/${finalAttrs.version}/ucm-macos-arm64.tar.gz";
        hash = "";
      };
      x86_64-linux = final.fetchurl {
        url = "https://github.com/unisonweb/unison/releases/download/release/${finalAttrs.version}/ucm-linux-x64.tar.gz";
        hash = "sha256-RKx5+6CsLvrTlLKreKSngPNG0Y0OommeUomXIc0ehsY=";
      };
    }.${final.stdenv.hostPlatform.system} or (throw "Unsupported platform ${final.stdenv.hostPlatform.system}");
  });
}
