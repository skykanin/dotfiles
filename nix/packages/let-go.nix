{
  name = "let-go";
  package = {
    buildGoModule,
    fetchFromGitHub,
    lib,
  }:
    buildGoModule rec {
      pname = "let-go";
      version = "1.7.2";

      src = fetchFromGitHub {
        owner = "nooga";
        repo = "let-go";
        rev = "v${version}";
        hash = "sha256-kKKYt03mHwe3xN6tsvOKlYKaJk3yrUGP3cGIAFYVmEA=";
      };

      vendorHash = "sha256-ZrSjzLAd4qEBkAWRS6vW+a5fZ79ecPETzwzT+HsHS60=";

      subPackages = [ "." ];

      ldflags = [
        "-s"
        "-w"
        "-X main.version=${version}"
        "-X main.commit=v${version}"
      ];

      doCheck = false;

      postInstall = ''
        mv "$out/bin/let-go" "$out/bin/lg"
      '';

      meta = with lib; {
        description = "Clojure dialect with a bytecode compiler and stack VM";
        homepage = "https://github.com/nooga/let-go";
        license = licenses.mit;
        mainProgram = "lg";
        platforms = platforms.unix;
        maintainers = with maintainers; [ skykanin ];
      };
    };
}
