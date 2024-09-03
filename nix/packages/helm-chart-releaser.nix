{
  name = "helm-chart-releaser";
  package = {
    buildGoModule,
    fetchFromGitHub,
    lib,
  }:
    buildGoModule rec {
      pname = "chart-releaser";
      version = "1.6.1";

      src = fetchFromGitHub {
        owner = "helm";
        repo = "chart-releaser";
        rev = "v${version}";
        hash = "sha256-8+O9JErEB1Z/zlrWm975v5Qf0YG0lbPcjY5LlDKw8U4=";
      };

      vendorHash = "sha256-S/V1kTgD/cXaJNYpPPNjM9ya2zv6Bsy/YBn7I/1EjwI=";

      doCheck = false;

      meta = {
        description = "Hosting Helm Charts via GitHub Pages and Releases";
        homepage = "https://github.com/helm/chart-releaser";
        platforms = lib.platforms.unix;
        license = lib.licenses.apsl20;
        maintainers = with lib.maintainers; [skykanin];
      };
    };
}
