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
        hash = "sha256-7XOB0YQ8VD3XvmtbhNBT3ymFnZhVn4TV3U6nTRMNdak=";
      };

      doCheck = false;

      cargoHash = "sha256-drJfyP8lH7IOrUNo7pBdSSZlANtw//S8yzQGnUc0YNY=";

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
