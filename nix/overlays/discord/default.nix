final: prev:
if prev.stdenv.hostPlatform.isDarwin
then {
  discord = final.stdenvNoCC.mkDerivation {
    pname = "discord";
    version = "0.0.410";

    src = final.fetchurl {
      url = "https://stable.dl2.discordapp.net/apps/osx/0.0.410/Discord.dmg";
      hash = "sha256-iT7HWLOuP9xvjJuxl2A9QQQr/zF6qn1hb46mQ1DZOxk=";
    };

    nativeBuildInputs = [final.undmg];
    sourceRoot = ".";
    dontFixup = true;

    installPhase = ''
      mkdir -p "$out/Applications" "$out/bin"
      cp -a Discord.app "$out/Applications/"
      ln -s ../Applications/Discord.app/Contents/MacOS/Discord "$out/bin/discord"
    '';

    inherit (prev.discord) meta;
  };
}
else {}
