{
  name = "cameracontroller";
  package = {
    fetchurl,
    stdenvNoCC,
    unzip,
    lib,
  }: stdenvNoCC.mkDerivation (finalAttrs: {
      pname = "cameracontroller";
      version = "1.4.0";
      src = fetchurl {
        url = "https://github.com/itaybre/CameraController/releases/download/v${finalAttrs.version}/CameraController.zip";
        hash = "sha256-ikbcsgqNiJjUxHVA9jbpkMo+NAHA/wYgQ+/lzDPTndo=";
      };

    nativeBuildInputs = [ unzip ];

    unpackPhase = ''
      unzip $src
    '';

    sourceRoot = "CameraController.app";
    dontPatch = true;
    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p $out/Applications/CameraController.app
      cp -R . $out/Applications/CameraController.app

      runHook postInstall
    '';

    meta = {
      description = "Control USB Cameras from an app";
      homepage = "https://github.com/Itaybre/CameraController";
      license = lib.licenses.gpl3Only;
      platforms = [ "aarch64-darwin" ];
      sourceProvenance = [lib.sourceTypes.binaryNativeCode];
    };

  });
}
