{
  name = "actions-languageserver";
  package = {
    buildNpmPackage,
    fetchFromGitHub,
    lib,
  }:
    buildNpmPackage rec {
      pname = "actions-languageserver";
      version = "0.3.58";

      src = fetchFromGitHub {
        owner = "actions";
        repo = "languageservices";
        rev = "release-v${version}";
        sparseCheckout = [
          "expressions"
          "languageservice"
          "languageserver"
          "package-lock.json"
          "package.json"
          "script"
          "workflow-parser"
        ];
        hash = "sha256-jvCxEtGIi1DJLXwKGnTbvegEDsyuTN5Box0rWsSPgM8=";
      };

      npmDepsHash = "sha256-AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA=";

      npmBuildScript = "build --workspace=@actions/languageserver";

      npmPackFlags = [ "--workspace=@actions/languageserver" ];

      meta = {
        description = "Language server for GitHub Actions";
        homepage = "https://github.com/actions/languageservices/tree/main/languageserver";
        license = lib.licenses.mit;
        mainProgram = "actions-languageserver";
        platforms = lib.platforms.unix;
        sourceProvenance = [lib.sourceTypes.fromSource];
      };
    };
}
