final: prev: if prev.stdenv.isDarwin then {
  signal-desktop = final.stdenvNoCC.mkDerivation (finalAttrs: {
    pname = "signal-desktop";
    version = "7.89.0";

    src = final.fetchurl {
      url = "https://updates.signal.org/desktop/signal-desktop-mac-universal-${finalAttrs.version}.dmg";
      hash = "sha256-Y2xUCWDRsb+A2GypvxZPc1VChRX3ElgrPOeeu4w4FRU=";
    };

    sourceRoot = ".";

    # Don't break codesigning
    dontFixup = true;
    dontPatch = true;
    dontConfigure = true;
    dontBuild = true;

    unpackCmd = ''
      echo "Creating temp directory"
      mnt=$(TMPDIR=/tmp mktemp -d -t nix-XXXXXXXXXX)
      function finish {
        echo "Ejecting temp directory"
        /usr/bin/hdiutil detach $mnt -force
        rm -rf $mnt
      }
      # Detach volume when receiving SIG "0"
      trap finish EXIT
      # Mount DMG file
      echo "Mounting DMG file into \"$mnt\""
      /usr/bin/hdiutil attach -nobrowse -mountpoint $mnt $curSrc
      # Copy content to local dir for later use
      echo 'Copying extracted content into "sourceRoot"'
      cp -a $mnt/Signal.app $PWD/
    '';

    nativeBuildInputs = [final.undmg];

    installPhase = ''
      runHook preInstall

      mkdir -p $out/Applications
      cp -r "Signal.app" $out/Applications

      runHook postInstall
    '';

    meta = {
      description = "Signal private messaging app";
      homepage = "https://signal.org/download/";
      license = final.lib.licenses.agpl3Only;
      platforms = final.lib.platforms.darwin;
      sourceProvenance = with final.lib.sourceTypes; [ binaryNativeCode ];
    };
  });

} else {}
