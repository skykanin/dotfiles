{
  name = "orion-browser";
  package = {
    stdenvNoCC,
    lib,
    fetchurl,
    undmg,
  }:
    stdenvNoCC.mkDerivation (finalAttrs: {
      pname = "orion-browser";
      version = "1.0.8";

      src = fetchurl {
        url = "https://cdn.kagi.com/downloads/26_0/Orion.dmg";
        name = "Orion-${finalAttrs.version}.dmg";
        hash = "sha256-Nvg+CjXNj+oayu2Suvjp05naZwRc2Buc5KJuHj2k5AM=";
      };

      sourceRoot = "Orion.app";

      nativeBuildInputs = [undmg];

      installPhase = ''
        runHook preInstall

        mkdir -p "$out/Applications/Orion.app"
        cp -R . "$out/Applications/Orion.app"
        mkdir "$out/bin"
        ln -s "$out/Applications/Orion.app/Contents/MacOS/Orion" "$out/bin/orion"

        runHook postInstall
      '';

      meta = with lib; {
        description = "Native macOS browser by Kagi";
        homepage = "https://kagi.com/orion";
        license = licenses.unfree;
        longDescription = ''
          Native macOS browser by Kagi. Incredibly fast. Built-in ad blocker.
          Zero telemetry. Web extensions support.
        '';
        mainProgram = "orion";
        platforms = platforms.darwin;
        sourceProvenance = [sourceTypes.binaryNativeCode];
      };
    });
}
