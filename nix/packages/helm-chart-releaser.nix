{
  name = "helm-chart-releaser";
  package = {
    buildGo127Module,
    fetchFromGitHub,
    lib,
  }:
    buildGo127Module rec {
      pname = "chart-releaser";
      version = "1.10.1";

      src = fetchFromGitHub {
        owner = "helm";
        repo = "chart-releaser";
        rev = "v${version}";
        hash = "sha256-RLlKRD690C3LTIGhmWNNlN6dRuTRCHu0QstufZ6+JBs=";
      };

      vendorHash = "sha256-M0opZb4s5A/zynI2V4wl5FK2pILtL2dehDmGc/htXvQ=";

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
