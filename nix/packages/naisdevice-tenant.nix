{
  # TODO: create a MacOS Application 'naisdevice.app' which starts
  # the naisdevice-helper and naisdevice-systray binaries
  name = "naisdevice-tenant";
  package = {
    buildGo126Module,
    fetchFromGitHub,
    lib,
    wireguard-go,
    wireguard-tools,
  }:
    let
      wgPath = lib.getExe' wireguard-tools "wg";
      wireguardGoPath = lib.getExe' wireguard-go "wireguard-go";
    in
    buildGo126Module rec {
      pname = "naisdevice-tenant";
      version = "1.12.0";
      rev = "v${version}";

      src = fetchFromGitHub {
        owner = "nais";
        repo = "device";
        rev = rev;
        hash = "sha256-5oQMExqkhyIsKGqG+yvuwfHJdhw7ewlYHkpqS11Gxug=";
      };

      patches = [ ./naisdevice-tenant-wireguard-paths.patch ];

      postPatch = ''
        substituteInPlace internal/helper/helper_darwin.go \
          --replace-fail "__WG_PATH__" "${wgPath}" \
          --replace-fail "__WIREGUARD_GO_PATH__" "${wireguardGoPath}"

        substituteInPlace cmd/naisdevice-systray/deviceagent_darwin.go \
          --replace-fail "/Applications/naisdevice.app/Contents/MacOS/naisdevice-agent" "${placeholder "out"}/bin/naisdevice-agent"
      '';

      vendorHash = "sha256-QmsrzXIVvmXHke62Iidem8CvoLuR0lB4uDOhr8ax114=";

      subPackages = [
        "cmd/naisdevice-helper"
        "cmd/naisdevice-systray"
        "cmd/naisdevice-agent"
      ];

      tags = [ "tenant" ];

      ldflags = [
        "-X github.com/nais/device/internal/version.Revision=${rev}"
        "-X github.com/nais/device/internal/version.Version=${version}"
        "-X github.com/nais/device/internal/otel.endpointURL=https://collector-internet.nav.cloud.nais.io"
      ];

      # TODO: Use write-darwin-bundle script to create PList information
      # and copy icon from assets/macos folder in the $src
      postInstall = ''
        app="$out/Applications/naisdevice.app"
        mkdir -p "$app/Contents/{MacOS, Resources}"

        cat > "$app/Contents/MacOS/naisdevice" <<'EOF'
#!/bin/sh
exec "${placeholder "out"}/bin/naisdevice-helper" &
exec "${placeholder "out"}/bin/naisdevice-systray"
EOF
        chmod 755 "$app/Contents/MacOS/naisdevice"
      '';

      meta = {
        description = "NAIS services VPN client";
        homepage = "https://doc.nais.io/operate/naisdevice";
        license = lib.licenses.mit;
        longDescription = ''
          Naisdevice is a application suite that enables NAV developers to connect to internal resources in a secure and friendly manner.
        '';
        platforms = [ "aarch64-darwin" ];
      };
    };
}
