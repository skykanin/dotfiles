{
  name = "naisdevice-tenant";
  package = {
    fetchurl,
    stdenvNoCC,
    cpio,
    gzip,
    xar,
    lib,
  }: stdenvNoCC.mkDerivation (finalAttrs: {
      pname = "naisdevice-tenant";
      version = "1.12.0";
      src = fetchurl {
        url = "https://github.com/nais/device/releases/download/v${finalAttrs.version}/naisdevice-tenant_macos_arm64.pkg";
        hash = "sha256-i3RgdgPaj/z0+MolTA/ijCpcHMuwfN9U/L5sNehScXk=";
      };


    nativeBuildInputs = [cpio gzip xar];

    unpackPhase = ''
      xar -xf $src
      gzip -dc Payload | cpio -i
    '';

    sourceRoot = "naisdevice.app";
    dontPatch = true;
    dontConfigure = true;
    dontBuild = true;

    installPhase = ''
      runHook preInstall

      mkdir -p $out/Applications/naisdevice.app
      cp -R . $out/Applications/naisdevice.app

      runHook postInstall
    '';

    meta = {
      description = "NAIS services VPN client";
      homepage = "https://doc.nais.io/operate/naisdevice";
      license = lib.licenses.mit;
      longDescription = ''
        Naisdevice is a application suite that enables NAV developers to connect to internal resources in a secure and friendly manner.
      '';
      platforms = [ "aarch64-darwin" ];
      sourceProvenance = [lib.sourceTypes.binaryNativeCode];
    };

  });
}
