{
  name = "httpie-desktop";
  package = {
    makeDesktopItem,
    copyDesktopItems,
    appimageTools,
    fetchurl,
    stdenvNoCC,
    lib,
  }: let
    pname = "httpie";
    version = "2024.1.2";
    src = fetchurl {
      url = "https://github.com/httpie/desktop/releases/download/v${version}/HTTPie-${version}.AppImage";
      hash = "sha256-OOP1l7J2BgO3nOPSipxfwfN/lOUsl80UzYMBosyBHrM=";
    };
    appimage = appimageTools.wrapType2 {inherit pname version src;};
    appimage-contents = appimageTools.extractType2 {inherit pname version src;};
  in
    stdenvNoCC.mkDerivation {
      inherit pname version;

      src = appimage;

      nativeBuildInputs = [copyDesktopItems];

      desktopItems = [
        (makeDesktopItem {
          name = pname;
          exec = pname;
          icon = "${appimage-contents}/${pname}";
          desktopName = "HTTPIE";
          categories = ["Utility"];
          type = "Application";
        })
      ];

      installPhase = ''
        runHook preInstall

        install -Dm 544 bin/* -t $out/bin

        runHook postInstall
      '';
    };
}
