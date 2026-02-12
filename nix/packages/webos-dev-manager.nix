{
  name = "webos-dev-manager";
  package = {
    appimageTools,
    fetchurl,
    lib,
  }:
    let version = "1.99.16";
    in appimageTools.wrapType2 {
      pname = "webos-dev-manager";
      inherit version;

      src = fetchurl {
        url = "https://github.com/webosbrew/dev-manager-desktop/releases/download/v${version}/webos-dev-manager_${version}_amd64.AppImage";
        hash = "sha256-1Eg8flL81vJXcGG9492tePqI4LpvEap2spuYtfIwAKU=";
      };

      meta = with lib; {
        description = "Device/DevMode Manager for webOS TV";
        homepage = "https://github.com/webosbrew/dev-manager-desktop";
        license = licenses.asl20;
        mainProgram = "dev-manager-desktop";
        platforms = platforms.linux;
        sourceProvenance = [sourceTypes.binaryNativeCode];
      };
    };
}
