{
  name = "cplt";
  package = {
    fetchFromGitHub,
    gitMinimal,
    lib,
    rustPlatform,
  }:
    rustPlatform.buildRustPackage rec {
      pname = "cplt";
      version = "2026.09.04-081715-4b9a2d1";

      src = fetchFromGitHub {
        owner = "navikt";
        repo = "cplt";
        rev = version;
        hash = "sha256-vkuPVdQ+4apucIVXPsY7yAx3ek1+FfoqXxh4SPo550U=";
      };

      cargoHash = "sha256-eP41X/0WvIDJM+vD4iBTFfSe9DiKdInpYs+hnaJ3G1k=";

      env.CPLT_LONG_VERSION = version;

      nativeCheckInputs = [gitMinimal];
      cargoTestFlags = ["--lib" "--bins"];

      meta = {
        description = "Kernel-enforced sandbox for AI coding agents";
        homepage = "https://github.com/navikt/cplt";
        license = lib.licenses.mit;
        mainProgram = "cplt";
        platforms = lib.platforms.unix;
      };
    };
}
