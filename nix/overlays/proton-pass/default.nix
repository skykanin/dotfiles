final: prev:
  if prev.stdenv.isDarwin then {
    proton-pass = final.stdenvNoCC.mkDerivation (finalAttrs: {
      pname = "proton-pass";
      version = "1.34.1";

      src = final.fetchurl {
        url = "https://proton.me/download/pass/macos/ProtonPass_${finalAttrs.version}.dmg";
        hash = "sha256-B5weoPQSnp4UT6bX5kNwb7dFixN1AV9TVSlC7niALwk=";
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
        cp -a $mnt/Proton\ Pass.app $PWD/
      '';

      nativeBuildInputs = [final.undmg];

      installPhase = ''
        runHook preInstall

        mkdir -p $out/Applications
        cp -r "Proton Pass.app" $out/Applications

        runHook postInstall
      '';

      meta = {
        description = "Desktop application for Proton Pass";
        homepage = "https://proton.me/pass";
        license = final.lib.licenses.gpl3Plus;
        maintainers = with final.lib.maintainers; [
          skykanin
        ];
        platforms = [ "aarch64-darwin" ];
        sourceProvenance = with final.lib.sourceTypes; [ binaryNativeCode ];
        mainProgram = "proton-pass";
      };
    });
  }
  else {}
